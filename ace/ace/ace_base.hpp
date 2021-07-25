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

namespace ace {

#if defined(_MSC_VER)
#define ACE_INLINE __forceinline
#elif defined(__clang__) || defined(__GNUC__)
#define ACE_INLINE __attribute__((always_inline)) __attribute__((unused)) inline
#else
#define ACE_INLINE inline
#endif

#ifdef __GNUC__
#define ACE_PRINTF_FORMATTING(x, y) __attribute__((format(printf, x, y)))
#else
#define ACE_PRINTF_FORMATTING(x, y)
#endif

#define ACE_ROUND_UP(to, x) ((((x) + (to)-1) / (to)) * (to))

#define ACE_STR(x) #x

#define ACE_ASSERT(x)                                                          \
    do {                                                                       \
        if (!(x)) {                                                            \
            fprintf(                                                           \
                stderr,                                                        \
                "ACE assertion failure: '%s' at %s:%d\n",                      \
                ACE_STR(x),                                                    \
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

#if __GNUC__ >= 9
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Winit-list-lifetime"
#endif
    Slice(const std::initializer_list<T> &list)
        : ptr(list.begin() == list.end() ? nullptr : (T *)list.begin()),
          len(list.size())
    {
    }
#if __GNUC__ >= 9
#pragma GCC diagnostic pop
#endif

    T &operator[](size_t index)
    {
        ACE_ASSERT(index < this->len);
        return this->ptr[index];
    }

    const T &operator[](size_t index) const
    {
        ACE_ASSERT(index < this->len);
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
        ACE_ASSERT(index < this->len);
        return this->ptr[index];
    }

    bool operator==(const String &other) const
    {
        if (this->len != other.len) return false;
        return (memcmp(this->ptr, other.ptr, this->len) == 0);
    }

    bool operator!=(const String &other) const
    {
        if (this->len != other.len) return true;
        return (memcmp(this->ptr, other.ptr, this->len) != 0);
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

    template <typename T> void free(T *ptr)
    {
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
        if (!str.ptr) return {};

        String new_str;
        new_str.ptr = (char *)this->alloc_bytes(str.len + 1);
        new_str.len = str.len;
        memcpy(new_str.ptr, str.ptr, str.len);
        new_str.ptr[new_str.len] = '\0';
        return new_str;
    }

    ACE_PRINTF_FORMATTING(2, 3)
    String sprintf(const char *fmt, ...)
    {
        String str;

        va_list args;
        va_start(args, fmt);
        str.len = vsnprintf(NULL, 0, fmt, args);
        va_end(args);

        str.ptr = (char *)this->alloc_bytes(str.len + 1);

        va_start(args, fmt);
        vsnprintf(str.ptr, str.len + 1, fmt, args);
        va_end(args);

        str.ptr[str.len] = '\0';

        return str;
    }

    String vsprintf(const char *fmt, va_list args)
    {
        String str;

        va_list args_copy;
        va_copy(args_copy, args);
        str.len = vsnprintf(NULL, 0, fmt, args_copy);
        va_end(args_copy);

        str.ptr = (char *)this->alloc_bytes(str.len + 1);

        vsnprintf(str.ptr, str.len + 1, fmt, args);

        str.ptr[str.len] = '\0';

        return str;
    }

    const char *null_terminate(const String &str)
    {
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
        return ::malloc(size);
    }

    virtual void *realloc_bytes(void *ptr, size_t size) override
    {
        return ::realloc(ptr, size);
    }

    virtual void free_bytes(void *ptr) override
    {
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

    ACE_INLINE static Array create(Allocator *allocator)
    {
        return {nullptr, 0, 0, allocator};
    }

    ACE_INLINE void destroy()
    {
        this->allocator->free(this->ptr);
        this->cap = 0;
        this->len = 0;
    }

    ACE_INLINE Slice<T> as_slice()
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

    ACE_INLINE void resize(size_t new_len)
    {
        ZoneScoped;

        this->reserve(new_len);
        this->len = new_len;
    }

    ACE_INLINE void push_back(const T &value)
    {
        ZoneScoped;

        this->reserve(this->len + 1);
        this->ptr[this->len] = value;
        this->len++;
    }

    ACE_INLINE void push_many(const Slice<T> &slice)
    {
        ZoneScoped;

        this->reserve(this->len + slice.len);

        T *first = this->ptr + this->len;
        for (size_t i = 0; i < slice.len; ++i) {
            first[i] = slice.ptr[i];
        }

        this->len += slice.len;
    }

    ACE_INLINE void pop()
    {
        if (this->len > 0) {
            this->len--;
        }
    }

    ACE_INLINE T &operator[](size_t index)
    {
        ACE_ASSERT(index < this->len);
        return this->ptr[index];
    }

    ACE_INLINE T *last()
    {
        if (this->len == 0) {
            return nullptr;
        }

        return &this->ptr[this->len - 1];
    }

    ACE_INLINE T *begin() const
    {
        return this->ptr;
    }

    ACE_INLINE T *end() const
    {
        return &this->ptr[this->len];
    }
};

template <typename T> struct List {
    struct Node {
        T value;
        struct Node *next;
        struct Node *prev;
    };

    Node *first = nullptr;
    Node *last = nullptr;
    Allocator *allocator = MallocAllocator::get_instance();

    ACE_INLINE static List create(Allocator *allocator)
    {
        return {nullptr, nullptr, allocator};
    }

    void destroy()
    {
        Node *node = this->first;
        while (node) {
            Node *deleted_node = node;
            node = node->next;
            this->allocator->free(deleted_node);
        }
        this->first = nullptr;
        this->last = nullptr;
    }

    Node *push_back(const T &value)
    {
        ZoneScoped;

        Node *new_node = this->allocator->template alloc<Node>();
        new_node->value = value;

        if (!this->last) {
            ACE_ASSERT(!this->first);

            new_node->next = nullptr;
            new_node->prev = nullptr;

            this->first = new_node;
            this->last = new_node;
        } else {
            new_node->next = nullptr;
            new_node->prev = this->last;

            this->last->next = new_node;

            this->last = new_node;
        }

        return new_node;
    }

    Node *push_front(const T &value)
    {
        ZoneScoped;

        Node *new_node = this->allocator->template alloc<Node>();
        new_node->value = value;

        if (!this->first) {
            ACE_ASSERT(!this->last);

            new_node->next = nullptr;
            new_node->prev = nullptr;

            this->first = new_node;
            this->last = new_node;
        } else {
            new_node->next = this->first;
            new_node->prev = nullptr;

            this->first->prev = new_node;

            this->first = new_node;
        }

        return new_node;
    }

    Node *insert_after(Node *other_node, const T &value)
    {
        ZoneScoped;

        Node *new_node = this->allocator->template alloc<Node>();
        new_node->value = value;

        if (other_node == this->last) {
            this->last = new_node;
        }

        new_node->prev = other_node;
        new_node->next = other_node->next;

        if (other_node->next) {
            other_node->next->prev = new_node;
        }
        other_node->next = new_node;

        return new_node;
    }

    Node *insert_before(Node *other_node, const T &value)
    {
        ZoneScoped;

        Node *new_node = this->allocator->template alloc<Node>();
        new_node->value = value;

        if (other_node == this->first) {
            this->first = new_node;
        }

        new_node->prev = other_node->prev;
        new_node->next = other_node;

        if (other_node->prev) {
            other_node->prev->next = new_node;
        }
        other_node->prev = new_node;

        return new_node;
    }

    void remove(Node *node)
    {
        ZoneScoped;

        if (this->first == node) {
            this->first = node->next;
        }
        if (this->last == node) {
            this->last = node->prev;
        }

        if (node->next) {
            node->next->prev = node->prev;
        }
        if (node->prev) {
            node->prev->next = node->next;
        }
    }

    ACE_INLINE bool is_empty()
    {
        return this->first == nullptr;
    }

    struct Iterator {
        Node *node;

        ACE_INLINE Iterator &operator++()
        {
            this->node = this->node->next;
            return *this;
        }

        ACE_INLINE Node &operator*() const
        {
            return *node;
        }

        ACE_INLINE Node *operator->()
        {
            return node;
        }

        ACE_INLINE friend bool operator==(const Iterator &a, const Iterator &b)
        {
            return a.node == b.node;
        };

        ACE_INLINE friend bool operator!=(const Iterator &a, const Iterator &b)
        {
            return a.node != b.node;
        };
    };

    ACE_INLINE Iterator begin()
    {
        return Iterator{this->first};
    }

    ACE_INLINE Iterator end()
    {
        return Iterator{nullptr};
    }
};

struct StringBuilder {
    Array<char> array;

    ACE_INLINE static StringBuilder
    create(Allocator *allocator, size_t size = 1 << 16)
    {
        auto array = Array<char>::create(allocator);
        array.reserve(size);
        return {array};
    }

    ACE_INLINE void destroy()
    {
        this->array.destroy();
    }

    ACE_INLINE
    void reset() {
        this->array.len = 0;
    }

    String build_null_terminated(Allocator *allocator)
    {
        ZoneScoped;

        Slice<char> chars = allocator->alloc<char>(this->array.len + 1);
        memcpy(chars.ptr, this->array.ptr, this->array.len);
        chars[chars.len - 1] = '\0';
        return String{
            chars.ptr,
            chars.len,
        };
    }

    ACE_INLINE void append(const String &str)
    {
        ZoneScoped;

        size_t old_arr_len = this->array.len;
        this->array.resize(old_arr_len + str.len);
        memcpy(&this->array.ptr[old_arr_len], str.ptr, str.len);
    }

    ACE_INLINE void append(char c)
    {
        ZoneScoped;

        this->array.push_back(c);
    }

    ACE_PRINTF_FORMATTING(2, 3)
    void sprintf(const char *fmt, ...)
    {
        ZoneScoped;

        va_list args;
        va_start(args, fmt);
        size_t str_len = vsnprintf(NULL, 0, fmt, args);
        va_end(args);

        size_t old_arr_len = this->array.len;
        this->array.reserve(old_arr_len + str_len + 1);
        this->array.resize(old_arr_len + str_len);

        va_start(args, fmt);
        vsnprintf(&this->array.ptr[old_arr_len], str_len + 1, fmt, args);
        va_end(args);
    }
};

template <typename T> struct StringMap {
    struct Slot {
        String key;
        uint64_t hash;
        T value;
    };

    Array<List<Slot>> slot_lists;

  private:
    ACE_INLINE static uint64_t string_hash(const char *string, size_t len)
    {
        uint64_t hash = 14695981039346656037ULL;
        for (const char *c = string; c != string + len; ++c) {
            hash = ((hash)*1099511628211) ^ (*c);
        }
        return hash;
    }

  public:
    static StringMap create(Allocator *allocator, size_t size)
    {
        // Round size to next power of 2
        size -= 1;
        size |= size >> 1;
        size |= size >> 2;
        size |= size >> 4;
        size |= size >> 8;
        size |= size >> 16;
        size |= size >> 32;
        size += 1;

        StringMap map;
        map.slot_lists = Array<List<Slot>>::create(allocator);
        map.slot_lists.resize(size);
        for (auto &slot_list : map.slot_lists) {
            slot_list = List<Slot>::create(allocator);
        }
        return map;
    }

    void destroy()
    {
        for (auto &slot_list : this->slot_lists) {
            slot_list.destroy();
        }
        this->slot_lists.destroy();
    }

    void set(String key, const T &value)
    {
        ZoneScoped;

        if (key.len == 0) return;

        uint64_t hash = string_hash(key.ptr, key.len);
        uint64_t i = hash & (this->slot_lists.len - 1); // fast modulo 2

        Slot slot = {key, hash, value};

        List<Slot> *slot_list = &this->slot_lists[i];
        for (auto &existing_slot_node : *slot_list) {
            auto existing_slot = &existing_slot_node.value;
            if (slot.hash == existing_slot->hash &&
                slot.key == existing_slot->key) {
                existing_slot->value = value;
                return;
            }
        }

        slot_list->push_back(slot);
    }

    bool get(String key, T *out_value = nullptr)
    {
        ZoneScoped;

        if (key.len == 0) return false;

        uint64_t hash = string_hash(key.ptr, key.len);
        uint64_t i = hash & (this->slot_lists.len - 1); // fast modulo 2

        List<Slot> *slot_list = &this->slot_lists[i];
        for (auto &existing_slot_node : *slot_list) {
            auto existing_slot = &existing_slot_node.value;
            if (hash == existing_slot->hash && key == existing_slot->key) {
                if (out_value) *out_value = existing_slot->value;
                return true;
            }
        }

        return false;
    }

    void remove(String key)
    {
        ZoneScoped;

        if (key.len == 0) return;

        uint64_t hash = string_hash(key.ptr, key.len);
        uint64_t i = hash & (this->slot_lists.len - 1); // fast modulo 2

        List<Slot> *slot_list = &this->slot_lists[i];
        for (auto &existing_slot_node : *slot_list) {
            auto existing_slot = &existing_slot_node.value;
            if (hash == existing_slot->hash && key == existing_slot->key) {
                slot_list->remove(&existing_slot_node);
                return;
            }
        }
    }
};

} // namespace ace
