#include "sir_base.hpp"

struct alignas(16) SIRArenaHeader {
    size_t size;
};

typedef struct SIRArenaChunk SIRArenaChunk;
struct SIRArenaChunk {
    SIRArenaChunk *prev;
    char *ptr;
    size_t size;
    size_t offset;
};

struct SIRArenaAllocator {
    SIRAllocator allocator;
    SIRAllocator *parent;
    SIRArenaChunk *last_chunk;
};

static SIRArenaChunk *
SIRArenaChunkCreate(SIRArenaAllocator *arena, SIRArenaChunk *prev, size_t size)
{
    SIRArenaChunk *chunk = SIRAlloc(arena->parent, SIRArenaChunk);
    *chunk = {};
    chunk->prev = prev;
    chunk->ptr = SIRAllocSlice(arena->parent, char, size);
    chunk->size = size;
    chunk->offset = 0;
    return chunk;
}

static void SIRArenaChunkDestroy(SIRArenaAllocator *arena, SIRArenaChunk *chunk)
{
    SIRFree(arena->parent, chunk->ptr);
    SIRFree(arena->parent, chunk);
}

static SIRArenaHeader *SIRArenaGetAllocHeader(void *ptr)
{
    return (SIRArenaHeader *)(((char *)ptr) - sizeof(SIRArenaHeader));
}

static void *SIRArenaAlloc(SIRAllocator *allocator, size_t size);
static void *SIRArenaRealloc(SIRAllocator *allocator, void *ptr, size_t size);
static void SIRArenaFree(SIRAllocator *allocator, void *ptr);

SIRArenaAllocator *
SIRArenaAllocatorCreate(SIRAllocator *parent_allocator)
{
    SIR_ASSERT(parent_allocator);

    SIRArenaAllocator *arena = SIRAlloc(parent_allocator, SIRArenaAllocator);
    *arena = {};
    arena->allocator.alloc = SIRArenaAlloc;
    arena->allocator.realloc = SIRArenaRealloc;
    arena->allocator.free = SIRArenaFree;
    arena->parent = parent_allocator;
    arena->last_chunk = SIRArenaChunkCreate(arena, NULL, 1 << 16 /* default size */);
    return arena;
}

void SIRArenaAllocatorDestroy(SIRArenaAllocator *arena)
{
    SIRArenaChunk *chunk = arena->last_chunk;
    while (chunk) {
        SIRArenaChunk *deleted_chunk = chunk;
        chunk = chunk->prev;
        SIRArenaChunkDestroy(arena, deleted_chunk);
    }
    SIRFree(arena->parent, arena);
}

static void *SIRArenaAlloc(SIRAllocator *allocator, size_t size)
{
    ZoneScoped;

    SIRArenaAllocator *arena = (SIRArenaAllocator *)allocator;

    SIRArenaChunk *chunk = arena->last_chunk;

    size_t new_offset = chunk->offset;
    while (new_offset % sizeof(SIRArenaHeader) != 0)
        new_offset++;
    new_offset += sizeof(SIRArenaHeader); // Header
    size_t data_offset = new_offset;
    new_offset += size;

    if (chunk->size <= new_offset) {
        size_t new_chunk_size = chunk->size * 2;
        while (new_chunk_size <= (size + 16))
            new_chunk_size *= 2;
        arena->last_chunk = SIRArenaChunkCreate(arena, chunk, new_chunk_size);
        return SIRArenaAlloc(&arena->allocator, size);
    }

    char *ptr = &chunk->ptr[data_offset];
    SIRArenaHeader *header = SIRArenaGetAllocHeader(ptr);
    header->size = size;

    chunk->offset = new_offset;

    return (void *)ptr;
}

static void *SIRArenaRealloc(SIRAllocator *allocator, void *ptr, size_t size)
{
    ZoneScoped;

    if (ptr) {
        SIRArenaHeader *header = SIRArenaGetAllocHeader(ptr);
        if (size <= header->size) return ptr;

        void *new_ptr = SIRArenaAlloc(allocator, size);
        memcpy(new_ptr, ptr, header->size);
        return new_ptr;
    } else {
        return SIRArenaAlloc(allocator, size);
    }
}

static void SIRArenaFree(SIRAllocator *allocator, void *ptr)
{
    ZoneScoped;
    (void)allocator;
    (void)ptr;
}
