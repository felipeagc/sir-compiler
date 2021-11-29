# SIR compiler

This is a compiler for a simple systems programming language (low level like C)
that compiles to a custom IR called SIR (simple IR). SIR can be compiled to x86-64
machine code (currently only AOT compilation is supported, JIT is planned).
SIR is also designed to be used as a standalone library through a C interface.

The goal of SIR is mainly compilation speed, not optimization (although some simple
optimizations are planned). Another goal is for the compiler to be mostly dependency-free,
depending only on the C standard library.

Current compile time benchmark results on a Ryzen 5 1600 are around 500k lines per second
running the `benchmark.sh` script.

## SIR Status

- [ ] System V calling convention
  - [x] Int register parameters
  - [x] SSE register parameters
  - [x] Stack parameters
  - [x] Int register returns
  - [x] SSE register returns
  - [ ] Larger than 16-byte function returns through stack
- [x] ELF64 object generation
- [ ] Linear scan register allocation

## Example

Source code:
```
fn extern vararg printf(fmt: *u8);

type Struct struct {
    a: i32,
    b: i32,
};

fn export main() {
    var a: Struct = undefined;
    a.a = 1;
    a.b = 2;
    printf("%d\n", a.a + a.b);
}
```

Generated IR:
```
%r6 = const_int @uint(32) 0
%r8 = const_int @int(32) 1
%r10 = const_int @uint(32) 1
%r12 = const_int @int(32) 2
%r15 = const_int @uint(32) 0
%r17 = const_int @uint(32) 1

%r14 = global (@ptr(@uint(8)))

function %r2 [system_v, external] "printf" (%r1: @ptr(@uint(8))) -> @void {
}

function %r3 [system_v, external] "main" () -> @void {
  %r5: stack_slot @ptr(@named_struct(@named_type(Struct)))

  block %b4:
    %r7 = struct_elem_ptr @ptr(@int(32)) %r5 %r6
    store @int(32) %r7 %r8
    %r11 = struct_elem_ptr @ptr(@int(32)) %r5 %r10
    store @int(32) %r11 %r12
    %r16 = struct_elem_ptr @ptr(@int(32)) %r5 %r15
    %r18 = struct_elem_ptr @ptr(@int(32)) %r5 %r17
    %r19 = load @int(32) %r16
    %r20 = load @int(32) %r18
    %r21 = iadd @int(32) %r19 %r20
    push_func_param %r14
    push_func_param %r21
    %r24 = func_call %r2
    return_void
}
```

Generated machine code:
```
0000000000000000 <main>:
   0:	   55                   	push   rbp
   1:	   48 89 e5             	mov    rbp,rsp
   4:	   48 83 ec 40          	sub    rsp,0x40
   8:	   c7 45 f8 01 00 00 00 	mov    DWORD PTR [rbp-0x8],0x1
   f:	   c7 45 fc 02 00 00 00 	mov    DWORD PTR [rbp-0x4],0x2
  16:	   8b 45 f8             	mov    eax,DWORD PTR [rbp-0x8]
  19:	   89 45 d4             	mov    DWORD PTR [rbp-0x2c],eax
  1c:	   8b 45 fc             	mov    eax,DWORD PTR [rbp-0x4]
  1f:	   89 45 d0             	mov    DWORD PTR [rbp-0x30],eax
  22:	   8b 45 d4             	mov    eax,DWORD PTR [rbp-0x2c]
  25:	   89 45 cc             	mov    DWORD PTR [rbp-0x34],eax
  28:	   03 45 d0             	add    eax,DWORD PTR [rbp-0x30]
  2b:	   89 45 cc             	mov    DWORD PTR [rbp-0x34],eax
  2e:	   48 8d 3d 00 00 00 00 	lea    rdi,[rip+0x0]        # 35 <main+0x35>
  35:	   8b 75 cc             	mov    esi,DWORD PTR [rbp-0x34]
  38:	   b8 00 00 00 00       	mov    eax,0x0
  3d:	/-- e8 00 00 00 00       	call   42 <main+0x42>
  42:	\-> c9                   	leave
  43:	   c3                   	ret
```
