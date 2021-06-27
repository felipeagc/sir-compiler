#include "ace_base.hpp"

using namespace ace;

ArenaAllocator::Chunk *
ArenaAllocator::Chunk::create(ArenaAllocator *arena, Chunk *prev, size_t size)
{
    ArenaAllocator::Chunk *chunk = arena->parent->alloc<Chunk>();
    new (chunk) Chunk();
    chunk->prev = prev;
    chunk->ptr = (char *)arena->parent->alloc_bytes(size);
    chunk->size = size;
    chunk->offset = 0;
    return chunk;
}

void ArenaAllocator::Chunk::destroy(ArenaAllocator *arena)
{
    arena->parent->free(this->ptr);
    arena->parent->free(this);
}

ArenaAllocator *ArenaAllocator::create(Allocator *parent, size_t size)
{
    ACE_ASSERT(parent);

    ArenaAllocator *arena = parent->alloc<ArenaAllocator>();
    new (arena) ArenaAllocator();
    arena->parent = parent;
    arena->last_chunk = Chunk::create(arena, nullptr, size);
    return arena;
}

void ArenaAllocator::destroy()
{
    Chunk *chunk = this->last_chunk;
    while (chunk) {
        Chunk *deleted_chunk = chunk;
        chunk = chunk->prev;
        deleted_chunk->destroy(this);
    }
    this->parent->free(this);
}

void *ArenaAllocator::alloc_bytes(size_t size)
{
    Chunk *chunk = this->last_chunk;

    size_t new_offset = chunk->offset;
    while (new_offset % sizeof(Header) != 0)
        new_offset++;
    new_offset += sizeof(Header); // Header
    size_t data_offset = new_offset;
    new_offset += size;

    if (chunk->size <= new_offset) {
        size_t new_chunk_size = chunk->size * 2;
        while (new_chunk_size <= (size + 16))
            new_chunk_size *= 2;
        this->last_chunk = Chunk::create(this, chunk, new_chunk_size);
        return this->alloc_bytes(size);
    }

    char *ptr = &chunk->ptr[data_offset];
    Header *header = get_alloc_header(ptr);
    header->size = size;

    chunk->offset = new_offset;

    return (void *)ptr;
}

void *ArenaAllocator::realloc_bytes(void *ptr, size_t size)
{
    if (ptr) {
        Header *header = get_alloc_header(ptr);
        if (size <= header->size) return ptr;

        void *new_ptr = this->alloc_bytes(size);
        memcpy(new_ptr, ptr, header->size);
        return new_ptr;
    } else {
        return this->alloc_bytes(size);
    }
}

void ArenaAllocator::free_bytes(void *ptr)
{
    (void)ptr;
}
