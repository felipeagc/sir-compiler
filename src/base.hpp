#pragma once

#include <string.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <new>
#include <Tracy.hpp>
#include "stb_sprintf.h"

#if defined(_MSC_VER)
#define LANG_INLINE __forceinline
#elif defined(__clang__) || defined(__GNUC__)
#define LANG_INLINE                                                            \
    __attribute__((always_inline)) __attribute__((unused)) inline
#else
#define LANG_INLINE inline
#endif

#ifdef __GNUC__
#define LANG_PRINTF_FORMATTING(x, y) __attribute__((format(printf, x, y)))
#else
#define LANG_PRINTF_FORMATTING(x, y)
#endif

#define LANG_ROUND_UP(to, x) ((((x) + (to)-1) / (to)) * (to))

#define LANG_CARRAY_LENGTH(arr) (sizeof(arr) / sizeof(arr[0]))

#define LANG_MACRO_STR(x) #x

#define LANG_ASSERT(x)                                                         \
    do {                                                                       \
        if (!(x)) {                                                            \
            fprintf(                                                           \
                stderr,                                                        \
                "Compiler assertion failure: '%s' at %s:%d\n",                 \
                LANG_MACRO_STR(x),                                             \
                __FILE__,                                                      \
                __LINE__);                                                     \
            abort();                                                           \
        }                                                                      \
    } while (0)

template <typename T> struct Slice {
    T *ptr = nullptr;
    size_t len = 0;

    Slice() : ptr(nullptr), len(0) {}

    Slice(T *ptr, size_t len) : ptr(ptr), len(len) {}

    template <size_t N>
    Slice(T (&arr)[N]) : ptr(&arr[0]), len(sizeof(arr) / sizeof(arr[0]))
    {
    }

    T &operator[](size_t index)
    {
        LANG_ASSERT(index < this->len);
        return this->ptr[index];
    }

    const T &operator[](size_t index) const
    {
        LANG_ASSERT(index < this->len);
        return this->ptr[index];
    }

    T *begin() const
    {
        return this->ptr;
    }

    T *end() const
    {
        return &this->ptr[this->len];
    }
};

struct String {
    char *ptr = nullptr;
    size_t len = 0;

    String()
    {
        this->ptr = 0;
        this->len = 0;
    }

    String(const char *str)
    {
        this->ptr = (char *)str;
        this->len = strlen(str);
    }

    String(const char *str, size_t len)
    {
        this->ptr = (char *)str;
        this->len = len;
    }

    char &operator[](size_t index) const
    {
        LANG_ASSERT(index < this->len);
        return this->ptr[index];
    }

    bool equal(const String &other) const
    {
        if (this->len != other.len) return false;
        return (memcmp(this->ptr, other.ptr, this->len) == 0);
    }

    char *begin() const
    {
        return this->ptr;
    }

    char *end() const
    {
        return &this->ptr[this->len];
    }
};

struct Allocator {
    virtual void *alloc_bytes(size_t size) = 0;
    virtual void *realloc_bytes(void *ptr, size_t size) = 0;
    virtual void free_bytes(void *ptr) = 0;

    template <typename T> T *alloc()
    {
        return static_cast<T *>(this->alloc_bytes(sizeof(T)));
    }

    template <typename T> T *alloc_init()
    {
        T *ptr = static_cast<T *>(this->alloc_bytes(sizeof(T)));
        new (ptr) T();
        return ptr;
    }

    template <typename T> T *realloc(T *ptr, size_t count)
    {
        return static_cast<T *>(this->realloc_bytes(ptr, sizeof(T) * count));
    }

    template <typename T> Slice<T> alloc(size_t len)
    {
        return Slice<T>{
            static_cast<T *>(this->alloc_bytes(sizeof(T) * len)),
            len,
        };
    }

    template <typename T> Slice<T> alloc_init(size_t len)
    {
        Slice<T> slice = {
            static_cast<T *>(this->alloc_bytes(sizeof(T) * len)),
            len,
        };
        for (size_t i = 0; i < len; ++i) {
            new (&slice.ptr[i]) T();
        }
        return slice;
    }

    template <typename T> void free(T *ptr)
    {
        ZoneScoped;
        this->free_bytes((void *)ptr);
    }

    template <typename T> void free(const Slice<T> &slice)
    {
        this->free_bytes(slice.ptr);
    }

    void free(const String &str)
    {
        this->free_bytes(str.ptr);
    }

    template <typename T> Slice<T> clone(const Slice<T> &slice)
    {
        if (!slice.ptr) return {};

        Slice<T> new_slice;
        new_slice.ptr = (T *)this->alloc_bytes(sizeof(T) * slice.len);
        new_slice.len = slice.len;
        memcpy((void *)new_slice.ptr, (void *)slice.ptr, sizeof(T) * slice.len);
        return new_slice;
    }

    String clone(const String &str)
    {
        ZoneScoped;

        if (!str.ptr) return {};

        String new_str;
        new_str.ptr = (char *)this->alloc_bytes(str.len + 1);
        new_str.len = str.len;
        memcpy(new_str.ptr, str.ptr, str.len);
        new_str.ptr[new_str.len] = '\0';
        return new_str;
    }

    LANG_PRINTF_FORMATTING(2, 3)
    String sprintf(const char *fmt, ...)
    {
        ZoneScoped;

        String str;

        va_list args;
        va_start(args, fmt);
        str.len = stbsp_vsnprintf(NULL, 0, fmt, args);
        va_end(args);

        str.ptr = (char *)this->alloc_bytes(str.len + 1);

        va_start(args, fmt);
        stbsp_vsnprintf(str.ptr, str.len + 1, fmt, args);
        va_end(args);

        str.ptr[str.len] = '\0';

        return str;
    }

    String vsprintf(const char *fmt, va_list args)
    {
        ZoneScoped;

        String str;

        va_list args_copy;
        va_copy(args_copy, args);
        str.len = stbsp_vsnprintf(NULL, 0, fmt, args_copy);
        va_end(args_copy);

        str.ptr = (char *)this->alloc_bytes(str.len + 1);

        stbsp_vsnprintf(str.ptr, str.len + 1, fmt, args);

        str.ptr[str.len] = '\0';

        return str;
    }

    const char *null_terminate(const String &str)
    {
        ZoneScoped;

        char *result = (char *)this->alloc_bytes(str.len + 1);
        memcpy(result, str.ptr, str.len);
        result[str.len] = '\0';
        return result;
    }
};

struct MallocAllocator : Allocator {
    static MallocAllocator *get_instance()
    {
        static MallocAllocator instance{};
        return &instance;
    }

    virtual void *alloc_bytes(size_t size) override
    {
        ZoneScoped;
        return ::malloc(size);
    }

    virtual void *realloc_bytes(void *ptr, size_t size) override
    {
        ZoneScoped;
        return ::realloc(ptr, size);
    }

    virtual void free_bytes(void *ptr) override
    {
        ZoneScoped;
        ::free(ptr);
    }
};

struct ArenaAllocator : Allocator {
  private:
    struct Chunk {
        struct Chunk *prev;
        char *ptr;
        size_t size;
        size_t offset;

        static Chunk *create(ArenaAllocator *arena, Chunk *prev, size_t size);
        void destroy(ArenaAllocator *arena);
    };

    struct alignas(16) Header {
        size_t size;
    };

    Allocator *parent;
    Chunk *last_chunk;

    static Header *get_alloc_header(void *ptr)
    {
        return (Header *)(((char *)ptr) - sizeof(Header));
    }

  public:
    static ArenaAllocator *create(Allocator *parent, size_t size = 1 << 16);
    void destroy();

    virtual void *alloc_bytes(size_t size) override;
    virtual void *realloc_bytes(void *ptr, size_t size) override;
    virtual void free_bytes(void *ptr) override;
};

template <typename T> struct Array {
    T *ptr = nullptr;
    size_t len = 0;
    size_t cap = 0;
    Allocator *allocator = MallocAllocator::get_instance();

    LANG_INLINE static Array create(Allocator *allocator)
    {
        return {nullptr, 0, 0, allocator};
    }

    LANG_INLINE void destroy()
    {
        this->allocator->free(this->ptr);
        this->cap = 0;
        this->len = 0;
    }

    LANG_INLINE Slice<T> as_slice()
    {
        return {this->ptr, this->len};
    }

    void reserve(size_t wanted_cap)
    {
        ZoneScoped;

        if (wanted_cap > this->cap) {
            this->cap *= 2;
            if (this->cap == 0) {
                this->cap = 16;
            }
            if (this->cap < wanted_cap) {
                this->cap = wanted_cap;
            }

            this->ptr = (T *)this->allocator->realloc_bytes(
                this->ptr, sizeof(T) * this->cap);
        }
    }

    LANG_INLINE void resize(size_t new_len)
    {
        ZoneScoped;

        this->reserve(new_len);
        this->len = new_len;
    }

    LANG_INLINE void push_back(const T &value)
    {
        ZoneScoped;

        this->reserve(this->len + 1);
        this->ptr[this->len] = value;
        this->len++;
    }

    LANG_INLINE void push_many(const Slice<T> &slice)
    {
        ZoneScoped;

        this->reserve(this->len + slice.len);

        T *first = this->ptr + this->len;
        for (size_t i = 0; i < slice.len; ++i) {
            first[i] = slice.ptr[i];
        }

        this->len += slice.len;
    }

    LANG_INLINE void push_many(const T *data, size_t len)
    {
        ZoneScoped;

        this->reserve(this->len + len);

        T *first = this->ptr + this->len;
        for (size_t i = 0; i < len; ++i) {
            first[i] = data[i];
        }

        this->len += len;
    }

    LANG_INLINE void pop()
    {
        if (this->len > 0) {
            this->len--;
        }
    }

    LANG_INLINE T &operator[](size_t index)
    {
        LANG_ASSERT(index < this->len);
        return this->ptr[index];
    }

    LANG_INLINE T *last()
    {
        if (this->len == 0) {
            return nullptr;
        }

        return &this->ptr[this->len - 1];
    }

    LANG_INLINE T *begin() const
    {
        return this->ptr;
    }

    LANG_INLINE T *end() const
    {
        return &this->ptr[this->len];
    }
};

struct StringBuilder {
    Array<char> array;

    LANG_INLINE static StringBuilder
    create(Allocator *allocator, size_t size = 1 << 16)
    {
        auto array = Array<char>::create(allocator);
        array.reserve(size);
        return {array};
    }

    LANG_INLINE void destroy()
    {
        this->array.destroy();
    }

    LANG_INLINE
    void reset()
    {
        this->array.len = 0;
    }

    String build_null_terminated(Allocator *allocator)
    {
        ZoneScoped;

        size_t str_byte_len = this->array.len + 1;
        char *str_ptr = allocator->alloc<char>(str_byte_len).ptr;
        memcpy(str_ptr, this->array.ptr, this->array.len);
        str_ptr[str_byte_len - 1] = '\0';
        return String{
            str_ptr,
            str_byte_len - 1,
        };
    }

    LANG_INLINE void append(const String &str)
    {
        ZoneScoped;

        size_t old_arr_len = this->array.len;
        this->array.resize(old_arr_len + str.len);
        memcpy(&this->array.ptr[old_arr_len], str.ptr, str.len);
    }

    LANG_INLINE void append(char c)
    {
        ZoneScoped;

        this->array.push_back(c);
    }

    LANG_PRINTF_FORMATTING(2, 3)
    void sprintf(const char *fmt, ...)
    {
        ZoneScoped;

        va_list args;
        va_start(args, fmt);
        size_t str_len = stbsp_vsnprintf(NULL, 0, fmt, args);
        va_end(args);

        size_t old_arr_len = this->array.len;
        this->array.reserve(old_arr_len + str_len + 1);
        this->array.resize(old_arr_len + str_len);

        va_start(args, fmt);
        stbsp_vsnprintf(&this->array.ptr[old_arr_len], str_len + 1, fmt, args);
        va_end(args);
    }
};

LANG_INLINE static uint64_t string_hash(const char *string, size_t len)
{
    uint64_t hash = 14695981039346656037ULL;
    for (const char *c = string; c != string + len; ++c) {
        hash = ((hash)*1099511628211ULL) ^ ((uint64_t)*c);
    }
    return hash;
}

static const int LANG_LOG2_TAB64[64] = {
    63, 0,  58, 1,  59, 47, 53, 2,  60, 39, 48, 27, 54, 33, 42, 3,
    61, 51, 37, 40, 49, 18, 28, 20, 55, 30, 34, 11, 43, 14, 22, 4,
    62, 57, 46, 52, 38, 26, 32, 41, 50, 36, 17, 19, 29, 10, 13, 21,
    56, 45, 25, 31, 35, 16, 9,  12, 44, 24, 15, 8,  23, 7,  6,  5};

LANG_INLINE static int log2_64(uint64_t value)
{
    ZoneScoped;
    value |= value >> 1;
    value |= value >> 2;
    value |= value >> 4;
    value |= value >> 8;
    value |= value >> 16;
    value |= value >> 32;
    return LANG_LOG2_TAB64
        [((uint64_t)((value - (value >> 1)) * 0x07EDD5E59A4E28C2)) >> 58];
}

template <typename T> struct StringMap {
    Allocator *allocator;
    size_t size;
    String *keys;
    uint64_t *hashes;
    T *values;

    static StringMap create(Allocator *allocator, size_t size = 16)
    {
        if (size == 0) size = 16;

        // Round size to next power of 2
        size -= 1;
        size |= size >> 1;
        size |= size >> 2;
        size |= size >> 4;
        size |= size >> 8;
        size |= size >> 16;
        size |= size >> 32;
        size += 1;

        StringMap map = {};
        map.allocator = allocator;
        map.size = size;
        map.keys = allocator->alloc_init<String>(map.size).ptr;
        map.hashes = allocator->alloc_init<uint64_t>(map.size).ptr;
        map.values = allocator->alloc<T>(map.size).ptr;

        return map;
    }

    void destroy()
    {
        this->allocator->free(this->keys);
        this->allocator->free(this->hashes);
        this->allocator->free(this->values);
        this->keys = nullptr;
        this->hashes = nullptr;
        this->values = nullptr;
    }

    void grow()
    {
        ZoneScoped;

        StringMap new_map = StringMap::create(this->allocator, this->size * 2);

        for (size_t i = 0; i < this->size; ++i) {
            if (this->keys[i].ptr) {
                T value;
                bool got_value = this->get(this->keys[i], &value);
                LANG_ASSERT(got_value);

                new_map.set(this->keys[i], value);
            }
        }

        this->destroy();
        *this = new_map;
    }

    void set(const String &key, const T &value)
    {
        ZoneScoped;

        if (key.len == 0) return;

        uint64_t hash = string_hash(key.ptr, key.len);

    start:
        uint64_t i = hash & (this->size - 1); // fast modulo for powers of 2

        size_t iters = 0;
        size_t max_iters = log2_64(this->size);

        while (this->keys[i].ptr != NULL &&
               (this->hashes[i] != hash || (!this->keys[i].equal(key)))) {
            iters++;
            if (iters > max_iters) break;

            i = (i + 1) & (this->size - 1); // fast modulo for powers of 2
        }

        if (iters > max_iters) {
            this->grow();
            goto start;
        }

        this->keys[i] = key;
        this->hashes[i] = hash;
        this->values[i] = value;
    }

    bool get(const String &key, T *out_value = nullptr)
    {
        ZoneScoped;

        if (key.len == 0) return false;

        uint64_t hash = string_hash(key.ptr, key.len);

        uint64_t i = hash & (this->size - 1); // fast modulo for powers of 2

        size_t iters = 0;
        size_t max_iters = log2_64(this->size);

        while (this->keys[i].ptr == NULL || this->hashes[i] != hash ||
               (!this->keys[i].equal(key))) {
            iters++;
            if (iters > max_iters) break;

            i = (i + 1) & (this->size - 1); // fast modulo for powers of 2
        }

        if (iters > max_iters) {
            return false;
        }

        if (out_value) *out_value = this->values[i];

        return true;
    }
};
