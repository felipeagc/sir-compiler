#pragma once

#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include <new>
#include <initializer_list>
#include <Tracy.hpp>
#include "stb_sprintf.h"

#if defined(_MSC_VER)
#define SIR_INLINE __forceinline
#elif defined(__clang__) || defined(__GNUC__)
#define SIR_INLINE __attribute__((always_inline)) __attribute__((unused)) inline
#else
#define SIR_INLINE inline
#endif

#ifdef __GNUC__
#define SIR_PRINTF_FORMATTING(x, y) __attribute__((format(printf, x, y)))
#else
#define SIR_PRINTF_FORMATTING(x, y)
#endif

#define SIR_ROUND_UP(to, x) ((((x) + (to)-1) / (to)) * (to))

#define SIR_CARRAY_LENGTH(arr) (sizeof(arr) / sizeof(arr[0]))

#define SIR_MACRO_STR(x) #x

#define SIR_ASSERT(x)                                                          \
    do {                                                                       \
        if (!(x)) {                                                            \
            fprintf(                                                           \
                stderr,                                                        \
                "ACE assertion failure: '%s' at %s:%d\n",                      \
                SIR_MACRO_STR(x),                                              \
                __FILE__,                                                      \
                __LINE__);                                                     \
            abort();                                                           \
        }                                                                      \
    } while (0)

template <typename T> struct SIRSlice {
    T *ptr = nullptr;
    size_t len = 0;

    SIRSlice() : ptr(nullptr), len(0) {}

    SIRSlice(T *ptr, size_t len) : ptr(ptr), len(len) {}

    template <size_t N>
    SIRSlice(T (&arr)[N]) : ptr(&arr[0]), len(sizeof(arr) / sizeof(arr[0]))
    {
    }

#if __GNUC__ >= 9
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Winit-list-lifetime"
#endif
    SIRSlice(const std::initializer_list<T> &list)
        : ptr(list.begin() == list.end() ? nullptr : (T *)list.begin()),
          len(list.size())
    {
    }
#if __GNUC__ >= 9
#pragma GCC diagnostic pop
#endif

    T &operator[](size_t index)
    {
        SIR_ASSERT(index < this->len);
        return this->ptr[index];
    }

    const T &operator[](size_t index) const
    {
        SIR_ASSERT(index < this->len);
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

struct SIRString {
    char *ptr = nullptr;
    size_t len = 0;

    SIRString()
    {
        this->ptr = 0;
        this->len = 0;
    }

    SIRString(const char *str)
    {
        this->ptr = (char *)str;
        this->len = strlen(str);
    }

    SIRString(const char *str, size_t len)
    {
        this->ptr = (char *)str;
        this->len = len;
    }

    char &operator[](size_t index) const
    {
        SIR_ASSERT(index < this->len);
        return this->ptr[index];
    }
};

SIR_INLINE static bool SIRStringEqual(SIRString str1, SIRString str2)
{
    ZoneScoped;
    if (str1.len != str2.len) return false;
    return (memcmp(str1.ptr, str2.ptr, str1.len) == 0);
}

struct SIRAllocator {
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

    template <typename T> SIRSlice<T> alloc(size_t len)
    {
        return SIRSlice<T>{
            static_cast<T *>(this->alloc_bytes(sizeof(T) * len)),
            len,
        };
    }

    template <typename T> SIRSlice<T> alloc_init(size_t len)
    {
        SIRSlice<T> slice = {
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

    template <typename T> void free(const SIRSlice<T> &slice)
    {
        this->free_bytes(slice.ptr);
    }

    void free(const SIRString &str)
    {
        this->free_bytes(str.ptr);
    }

    template <typename T> SIRSlice<T> clone(const SIRSlice<T> &slice)
    {
        if (!slice.ptr) return {};

        SIRSlice<T> new_slice;
        new_slice.ptr = (T *)this->alloc_bytes(sizeof(T) * slice.len);
        new_slice.len = slice.len;
        memcpy((void *)new_slice.ptr, (void *)slice.ptr, sizeof(T) * slice.len);
        return new_slice;
    }

    SIRString clone(const SIRString &str)
    {
        ZoneScoped;

        if (!str.ptr) return {};

        SIRString new_str;
        new_str.ptr = (char *)this->alloc_bytes(str.len + 1);
        new_str.len = str.len;
        memcpy(new_str.ptr, str.ptr, str.len);
        new_str.ptr[new_str.len] = '\0';
        return new_str;
    }

    SIR_PRINTF_FORMATTING(2, 3)
    SIRString sprintf(const char *fmt, ...)
    {
        ZoneScoped;

        SIRString str;

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

    SIRString vsprintf(const char *fmt, va_list args)
    {
        ZoneScoped;

        SIRString str;

        va_list args_copy;
        va_copy(args_copy, args);
        str.len = stbsp_vsnprintf(NULL, 0, fmt, args_copy);
        va_end(args_copy);

        str.ptr = (char *)this->alloc_bytes(str.len + 1);

        stbsp_vsnprintf(str.ptr, str.len + 1, fmt, args);

        str.ptr[str.len] = '\0';

        return str;
    }

    const char *null_terminate(const SIRString &str)
    {
        ZoneScoped;

        char *result = (char *)this->alloc_bytes(str.len + 1);
        memcpy(result, str.ptr, str.len);
        result[str.len] = '\0';
        return result;
    }
};

struct SIRMallocAllocator : SIRAllocator {
    static SIRMallocAllocator *get_instance()
    {
        static SIRMallocAllocator instance{};
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

struct SIRArenaAllocator : SIRAllocator {
  private:
    struct Chunk {
        struct Chunk *prev;
        char *ptr;
        size_t size;
        size_t offset;

        static Chunk *
        create(SIRArenaAllocator *arena, Chunk *prev, size_t size);
        void destroy(SIRArenaAllocator *arena);
    };

    struct alignas(16) Header {
        size_t size;
    };

    SIRAllocator *parent;
    Chunk *last_chunk;

    static Header *get_alloc_header(void *ptr)
    {
        return (Header *)(((char *)ptr) - sizeof(Header));
    }

  public:
    static SIRArenaAllocator *
    create(SIRAllocator *parent, size_t size = 1 << 16);
    void destroy();

    virtual void *alloc_bytes(size_t size) override;
    virtual void *realloc_bytes(void *ptr, size_t size) override;
    virtual void free_bytes(void *ptr) override;
};

template <typename T> struct SIRArray {
    T *ptr = nullptr;
    size_t len = 0;
    size_t cap = 0;
    SIRAllocator *allocator = SIRMallocAllocator::get_instance();

    SIR_INLINE static SIRArray create(SIRAllocator *allocator)
    {
        return {nullptr, 0, 0, allocator};
    }

    SIR_INLINE void destroy()
    {
        this->allocator->free(this->ptr);
        this->cap = 0;
        this->len = 0;
    }

    SIR_INLINE SIRSlice<T> as_slice()
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

            this->ptr = this->allocator->realloc(this->ptr, this->cap);
        }
    }

    SIR_INLINE void resize(size_t new_len)
    {
        ZoneScoped;

        this->reserve(new_len);
        this->len = new_len;
    }

    SIR_INLINE void push_back(const T &value)
    {
        ZoneScoped;

        this->reserve(this->len + 1);
        this->ptr[this->len] = value;
        this->len++;
    }

    SIR_INLINE void push_many(const SIRSlice<T> &slice)
    {
        ZoneScoped;

        this->reserve(this->len + slice.len);

        T *first = this->ptr + this->len;
        for (size_t i = 0; i < slice.len; ++i) {
            first[i] = slice.ptr[i];
        }

        this->len += slice.len;
    }

    SIR_INLINE void pop()
    {
        if (this->len > 0) {
            this->len--;
        }
    }

    SIR_INLINE T &operator[](size_t index)
    {
        SIR_ASSERT(index < this->len);
        return this->ptr[index];
    }

    SIR_INLINE T *last()
    {
        if (this->len == 0) {
            return nullptr;
        }

        return &this->ptr[this->len - 1];
    }

    SIR_INLINE T *begin() const
    {
        return this->ptr;
    }

    SIR_INLINE T *end() const
    {
        return &this->ptr[this->len];
    }
};

struct SIRStringBuilder {
    SIRArray<char> array;

    SIR_INLINE static SIRStringBuilder
    create(SIRAllocator *allocator, size_t size = 1 << 16)
    {
        auto array = SIRArray<char>::create(allocator);
        array.reserve(size);
        return {array};
    }

    SIR_INLINE void destroy()
    {
        this->array.destroy();
    }

    SIR_INLINE
    void reset()
    {
        this->array.len = 0;
    }

    SIRString build_null_terminated(SIRAllocator *allocator)
    {
        ZoneScoped;

        SIRSlice<char> chars = allocator->alloc<char>(this->array.len + 1);
        memcpy(chars.ptr, this->array.ptr, this->array.len);
        chars[chars.len - 1] = '\0';
        return SIRString{
            chars.ptr,
            chars.len - 1,
        };
    }

    SIR_INLINE void append(const SIRString &str)
    {
        ZoneScoped;

        size_t old_arr_len = this->array.len;
        this->array.resize(old_arr_len + str.len);
        memcpy(&this->array.ptr[old_arr_len], str.ptr, str.len);
    }

    SIR_INLINE void append(char c)
    {
        ZoneScoped;

        this->array.push_back(c);
    }

    SIR_PRINTF_FORMATTING(2, 3)
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

static const int SIR_LOG2_TAB64[64] = {
    63, 0,  58, 1,  59, 47, 53, 2,  60, 39, 48, 27, 54, 33, 42, 3,
    61, 51, 37, 40, 49, 18, 28, 20, 55, 30, 34, 11, 43, 14, 22, 4,
    62, 57, 46, 52, 38, 26, 32, 41, 50, 36, 17, 19, 29, 10, 13, 21,
    56, 45, 25, 31, 35, 16, 9,  12, 44, 24, 15, 8,  23, 7,  6,  5};

SIR_INLINE static int SIRLog2_64(uint64_t value)
{
    ZoneScoped;
    value |= value >> 1;
    value |= value >> 2;
    value |= value >> 4;
    value |= value >> 8;
    value |= value >> 16;
    value |= value >> 32;
    return SIR_LOG2_TAB64
        [((uint64_t)((value - (value >> 1)) * 0x07EDD5E59A4E28C2)) >> 58];
}

struct SIRStringMap {
    SIRAllocator *allocator;
    size_t size;
    SIRString *keys;
    uint64_t *hashes;
    uintptr_t *values;
};

static inline SIRStringMap
SIRStringMapCreate(SIRAllocator *allocator, size_t size);
static inline void SIRStringMapDestroy(SIRStringMap *map);
static inline void
SIRStringMapSet(SIRStringMap *map, SIRString key, const uintptr_t &value);
static inline bool
SIRStringMapGet(SIRStringMap *map, SIRString key, uintptr_t *out_value);

SIR_INLINE static uint64_t SIRStringHash(const char *string, size_t len)
{
    uint64_t hash = 14695981039346656037ULL;
    for (const char *c = string; c != string + len; ++c) {
        hash = ((hash)*1099511628211) ^ (*c);
    }
    return hash;
}

static inline SIRStringMap
SIRStringMapCreate(SIRAllocator *allocator, size_t size)
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

    SIRStringMap map = {};
    map.allocator = allocator;
    map.size = size;
    map.keys = allocator->alloc_init<SIRString>(map.size).ptr;
    map.hashes = allocator->alloc<uint64_t>(map.size).ptr;
    map.values = allocator->alloc<uintptr_t>(map.size).ptr;

    return map;
}

static inline void SIRStringMapDestroy(SIRStringMap *map)
{
    map->allocator->free(map->keys);
    map->allocator->free(map->hashes);
    map->allocator->free(map->values);
    map->size = 0;
    map->keys = NULL;
    map->hashes = NULL;
    map->values = NULL;
}

static inline void SIRStringMapGrow(SIRStringMap *map)
{
    ZoneScoped;

    SIRStringMap new_map = SIRStringMapCreate(map->allocator, map->size * 2);

    for (size_t i = 0; i < map->size; ++i) {
        if (map->keys[i].ptr) {
            uintptr_t value;
            bool got_value = SIRStringMapGet(map, map->keys[i], &value);
            SIR_ASSERT(got_value);

            SIRStringMapSet(&new_map, map->keys[i], value);
        }
    }

    SIRStringMapDestroy(map);
    *map = new_map;
}

static inline void
SIRStringMapSet(SIRStringMap *map, SIRString key, const uintptr_t &value)
{
    ZoneScoped;

    if (key.len == 0) return;

    uint64_t hash = SIRStringHash(key.ptr, key.len);

start:
    uint64_t i = hash & (map->size - 1); // fast modulo for powers of 2

    size_t iters = 0;
    size_t max_iters = SIRLog2_64(map->size);

    while (map->keys[i].ptr != nullptr &&
           (map->hashes[i] != hash || (!SIRStringEqual(map->keys[i], key)))) {
        iters++;
        if (iters > max_iters) break;

        i = (i + 1) & (map->size - 1); // fast modulo for powers of 2
    }

    if (iters > max_iters) {
        SIRStringMapGrow(map);
        goto start;
    }

    map->keys[i] = key;
    map->hashes[i] = hash;
    map->values[i] = value;
}

static inline bool
SIRStringMapGet(SIRStringMap *map, SIRString key, uintptr_t *out_value)
{
    ZoneScoped;

    if (key.len == 0) return false;

    uint64_t hash = SIRStringHash(key.ptr, key.len);

    uint64_t i = hash & (map->size - 1); // fast modulo for powers of 2

    size_t iters = 0;
    size_t max_iters = SIRLog2_64(map->size);

    while (map->keys[i].ptr == nullptr || map->hashes[i] != hash ||
           (!SIRStringEqual(map->keys[i], key))) {
        iters++;
        if (iters > max_iters) break;

        i = (i + 1) & (map->size - 1); // fast modulo for powers of 2
    }

    if (iters > max_iters) {
        return false;
    }

    if (out_value) *out_value = map->values[i];

    return true;
}
