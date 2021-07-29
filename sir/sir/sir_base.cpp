#include "sir_base.hpp"

SIRArenaAllocator::Chunk *
SIRArenaAllocator::Chunk::create(SIRArenaAllocator *arena, Chunk *prev, size_t size)
{
    SIRArenaAllocator::Chunk *chunk = arena->parent->alloc<Chunk>();
    new (chunk) Chunk();
    chunk->prev = prev;
    chunk->ptr = (char *)arena->parent->alloc_bytes(size);
    chunk->size = size;
    chunk->offset = 0;
    return chunk;
}

void SIRArenaAllocator::Chunk::destroy(SIRArenaAllocator *arena)
{
    arena->parent->free(this->ptr);
    arena->parent->free(this);
}

SIRArenaAllocator *SIRArenaAllocator::create(SIRAllocator *parent, size_t size)
{
    SIR_ASSERT(parent);

    SIRArenaAllocator *arena = parent->alloc<SIRArenaAllocator>();
    new (arena) SIRArenaAllocator();
    arena->parent = parent;
    arena->last_chunk = Chunk::create(arena, nullptr, size);
    return arena;
}

void SIRArenaAllocator::destroy()
{
    Chunk *chunk = this->last_chunk;
    while (chunk) {
        Chunk *deleted_chunk = chunk;
        chunk = chunk->prev;
        deleted_chunk->destroy(this);
    }
    this->parent->free(this);
}

void *SIRArenaAllocator::alloc_bytes(size_t size)
{
    ZoneScoped;

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

void *SIRArenaAllocator::realloc_bytes(void *ptr, size_t size)
{
    ZoneScoped;

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

void SIRArenaAllocator::free_bytes(void *ptr)
{
    ZoneScoped;
    (void)ptr;
}
