#pragma once

#include "sir.h"

#ifdef __cplusplus
extern "C" {
#endif

typedef struct SIRInterpContext SIRInterpContext;

typedef union SIRInterpValue {
    bool boolean;
    uint8_t u8;
    uint16_t u16;
    uint32_t u32;
    uint64_t u64;
    int8_t i8;
    int16_t i16;
    int32_t i32;
    int64_t i64;
    float f32;
    double f64;
    char *bytes;
} SIRInterpValue;

SIRInterpContext* SIRInterpContextCreate(SIRModule *mod);
void SIRInterpContextDestroy(SIRInterpContext *ctx);

void SIRInterpFunction(SIRInterpContext *ctx, SIRInstRef func_ref, void *result);

#ifdef __cplusplus
}
#endif
