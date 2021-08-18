#pragma once

#include "sir.h"

#ifdef __cplusplus
extern "C" {
#endif

typedef struct SIRInterpContext SIRInterpContext;

typedef enum SIRInterpResult {
    SIRInterpResult_Success = 0,
    SIRInterpResult_StackOverflow,
    SIRInterpResult_CannotBeInterpreted,
} SIRInterpResult;

SIRInterpContext* SIRInterpContextCreate(SIRModule *mod);
void SIRInterpContextDestroy(SIRInterpContext *ctx);

SIRInterpResult SIRInterpFunction(SIRInterpContext *ctx, SIRInstRef func_ref, void *result);

#ifdef __cplusplus
}
#endif
