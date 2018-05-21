
int32.o:     file format elf64-x86-64


Disassembly of section .text:

0000000000000000 <add_i32_alt>:
/*     _mm_store_si128(&vr_128[i], _mm_add_epi32(_mm_load_si128(&v1_128[i]), _mm_load_si128(&v2_128[i]))); */
/*     vr_128[i] = _mm_add_epi32(v1_128[i], v2_128[i]); */
/*   } */
/* } */

void add_i32_alt(int32_t vr_32[], const int32_t v1_32[], const int32_t v2_32[], const int n) {
   0:	55                   	push   rbp
   1:	48 89 e5             	mov    rbp,rsp
   4:	48 89 7d b8          	mov    QWORD PTR [rbp-0x48],rdi
   8:	48 89 75 b0          	mov    QWORD PTR [rbp-0x50],rsi
   c:	48 89 55 a8          	mov    QWORD PTR [rbp-0x58],rdx
  10:	89 4d a4             	mov    DWORD PTR [rbp-0x5c],ecx
  __m128i* vr_128 = (__m128i*) vr_32;
  13:	48 8b 45 b8          	mov    rax,QWORD PTR [rbp-0x48]
  17:	48 89 45 c8          	mov    QWORD PTR [rbp-0x38],rax
  __m128i* v1_128 = (__m128i*) v1_32;
  1b:	48 8b 45 b0          	mov    rax,QWORD PTR [rbp-0x50]
  1f:	48 89 45 d0          	mov    QWORD PTR [rbp-0x30],rax
  __m128i* v2_128 = (__m128i*) v2_32;
  23:	48 8b 45 a8          	mov    rax,QWORD PTR [rbp-0x58]
  27:	48 89 45 d8          	mov    QWORD PTR [rbp-0x28],rax
  for(int i = 0; i < n; i++){
  2b:	c7 45 c4 00 00 00 00 	mov    DWORD PTR [rbp-0x3c],0x0
  32:	eb 60                	jmp    94 <add_i32_alt+0x94>
    //_mm_store_si128(&vr_128[i], _mm_add_epi32(_mm_load_si128(&v1_128[i]), _mm_load_si128(&v2_128[i])));
    vr_128[i] = _mm_add_epi32(v1_128[i], v2_128[i]);
  34:	8b 45 c4             	mov    eax,DWORD PTR [rbp-0x3c]
  37:	48 98                	cdqe   
  39:	48 c1 e0 04          	shl    rax,0x4
  3d:	48 89 c2             	mov    rdx,rax
  40:	48 8b 45 c8          	mov    rax,QWORD PTR [rbp-0x38]
  44:	48 01 d0             	add    rax,rdx
  47:	8b 55 c4             	mov    edx,DWORD PTR [rbp-0x3c]
  4a:	48 63 d2             	movsxd rdx,edx
  4d:	48 89 d1             	mov    rcx,rdx
  50:	48 c1 e1 04          	shl    rcx,0x4
  54:	48 8b 55 d8          	mov    rdx,QWORD PTR [rbp-0x28]
  58:	48 01 ca             	add    rdx,rcx
  5b:	66 0f 6f 02          	movdqa xmm0,XMMWORD PTR [rdx]
  5f:	8b 55 c4             	mov    edx,DWORD PTR [rbp-0x3c]
  62:	48 63 d2             	movsxd rdx,edx
  65:	48 89 d1             	mov    rcx,rdx
  68:	48 c1 e1 04          	shl    rcx,0x4
  6c:	48 8b 55 d0          	mov    rdx,QWORD PTR [rbp-0x30]
  70:	48 01 ca             	add    rdx,rcx
  73:	66 0f 6f 0a          	movdqa xmm1,XMMWORD PTR [rdx]
  77:	0f 29 4d e0          	movaps XMMWORD PTR [rbp-0x20],xmm1
  7b:	0f 29 45 f0          	movaps XMMWORD PTR [rbp-0x10],xmm0
}

extern __inline __m128i __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_add_epi32 (__m128i __A, __m128i __B)
{
  return (__m128i) ((__v4su)__A + (__v4su)__B);
  7f:	66 0f 6f 4d e0       	movdqa xmm1,XMMWORD PTR [rbp-0x20]
  84:	66 0f 6f 45 f0       	movdqa xmm0,XMMWORD PTR [rbp-0x10]
  89:	66 0f fe c1          	paddd  xmm0,xmm1
  8d:	0f 29 00             	movaps XMMWORD PTR [rax],xmm0

void add_i32_alt(int32_t vr_32[], const int32_t v1_32[], const int32_t v2_32[], const int n) {
  __m128i* vr_128 = (__m128i*) vr_32;
  __m128i* v1_128 = (__m128i*) v1_32;
  __m128i* v2_128 = (__m128i*) v2_32;
  for(int i = 0; i < n; i++){
  90:	83 45 c4 01          	add    DWORD PTR [rbp-0x3c],0x1
  94:	8b 45 c4             	mov    eax,DWORD PTR [rbp-0x3c]
  97:	3b 45 a4             	cmp    eax,DWORD PTR [rbp-0x5c]
  9a:	7c 98                	jl     34 <add_i32_alt+0x34>
    //_mm_store_si128(&vr_128[i], _mm_add_epi32(_mm_load_si128(&v1_128[i]), _mm_load_si128(&v2_128[i])));
    vr_128[i] = _mm_add_epi32(v1_128[i], v2_128[i]);
  }
}
  9c:	90                   	nop
  9d:	5d                   	pop    rbp
  9e:	c3                   	ret    

000000000000009f <add_i32_simple>:

void add_i32_simple(int32_t vr_32[], const int32_t v1_32[], const int32_t v2_32[], const int n) {
  9f:	55                   	push   rbp
  a0:	48 89 e5             	mov    rbp,rsp
  a3:	48 89 7d e8          	mov    QWORD PTR [rbp-0x18],rdi
  a7:	48 89 75 e0          	mov    QWORD PTR [rbp-0x20],rsi
  ab:	48 89 55 d8          	mov    QWORD PTR [rbp-0x28],rdx
  af:	89 4d d4             	mov    DWORD PTR [rbp-0x2c],ecx
  for(int i = 0; i < n; i++){
  b2:	c7 45 fc 00 00 00 00 	mov    DWORD PTR [rbp-0x4],0x0
  b9:	eb 4a                	jmp    105 <add_i32_simple+0x66>
    vr_32[i] = v1_32[i] + v2_32[i];
  bb:	8b 45 fc             	mov    eax,DWORD PTR [rbp-0x4]
  be:	48 98                	cdqe   
  c0:	48 8d 14 85 00 00 00 	lea    rdx,[rax*4+0x0]
  c7:	00 
  c8:	48 8b 45 e8          	mov    rax,QWORD PTR [rbp-0x18]
  cc:	48 01 d0             	add    rax,rdx
  cf:	8b 55 fc             	mov    edx,DWORD PTR [rbp-0x4]
  d2:	48 63 d2             	movsxd rdx,edx
  d5:	48 8d 0c 95 00 00 00 	lea    rcx,[rdx*4+0x0]
  dc:	00 
  dd:	48 8b 55 e0          	mov    rdx,QWORD PTR [rbp-0x20]
  e1:	48 01 ca             	add    rdx,rcx
  e4:	8b 0a                	mov    ecx,DWORD PTR [rdx]
  e6:	8b 55 fc             	mov    edx,DWORD PTR [rbp-0x4]
  e9:	48 63 d2             	movsxd rdx,edx
  ec:	48 8d 34 95 00 00 00 	lea    rsi,[rdx*4+0x0]
  f3:	00 
  f4:	48 8b 55 d8          	mov    rdx,QWORD PTR [rbp-0x28]
  f8:	48 01 f2             	add    rdx,rsi
  fb:	8b 12                	mov    edx,DWORD PTR [rdx]
  fd:	01 ca                	add    edx,ecx
  ff:	89 10                	mov    DWORD PTR [rax],edx
    vr_128[i] = _mm_add_epi32(v1_128[i], v2_128[i]);
  }
}

void add_i32_simple(int32_t vr_32[], const int32_t v1_32[], const int32_t v2_32[], const int n) {
  for(int i = 0; i < n; i++){
 101:	83 45 fc 01          	add    DWORD PTR [rbp-0x4],0x1
 105:	8b 45 fc             	mov    eax,DWORD PTR [rbp-0x4]
 108:	3b 45 d4             	cmp    eax,DWORD PTR [rbp-0x2c]
 10b:	7c ae                	jl     bb <add_i32_simple+0x1c>
    vr_32[i] = v1_32[i] + v2_32[i];
  }
}
 10d:	90                   	nop
 10e:	5d                   	pop    rbp
 10f:	c3                   	ret    

0000000000000110 <main>:
/*     vr_32[i] = v1_32[i] + v2_32[i]; */
/*   } */
/* } */


int main(void){
 110:	55                   	push   rbp
 111:	48 89 e5             	mov    rbp,rsp
 114:	41 57                	push   r15
 116:	41 56                	push   r14
 118:	41 55                	push   r13
 11a:	41 54                	push   r12
 11c:	53                   	push   rbx
 11d:	48 83 ec 48          	sub    rsp,0x48
 121:	64 48 8b 04 25 28 00 	mov    rax,QWORD PTR fs:0x28
 128:	00 00 
 12a:	48 89 45 c8          	mov    QWORD PTR [rbp-0x38],rax
 12e:	31 c0                	xor    eax,eax
 130:	48 89 e0             	mov    rax,rsp
 133:	48 89 c3             	mov    rbx,rax
  const int n = 10000;
 136:	c7 45 94 10 27 00 00 	mov    DWORD PTR [rbp-0x6c],0x2710
  int32_t foo[n], bar[n], res[n];
 13d:	8b 45 94             	mov    eax,DWORD PTR [rbp-0x6c]
 140:	48 98                	cdqe   
 142:	48 83 e8 01          	sub    rax,0x1
 146:	48 89 45 98          	mov    QWORD PTR [rbp-0x68],rax
 14a:	8b 45 94             	mov    eax,DWORD PTR [rbp-0x6c]
 14d:	48 98                	cdqe   
 14f:	49 89 c6             	mov    r14,rax
 152:	41 bf 00 00 00 00    	mov    r15d,0x0
 158:	8b 45 94             	mov    eax,DWORD PTR [rbp-0x6c]
 15b:	48 98                	cdqe   
 15d:	48 89 c2             	mov    rdx,rax
 160:	b9 00 00 00 00       	mov    ecx,0x0
 165:	8b 45 94             	mov    eax,DWORD PTR [rbp-0x6c]
 168:	48 98                	cdqe   
 16a:	48 c1 e0 02          	shl    rax,0x2
 16e:	48 8d 50 03          	lea    rdx,[rax+0x3]
 172:	b8 10 00 00 00       	mov    eax,0x10
 177:	48 83 e8 01          	sub    rax,0x1
 17b:	48 01 d0             	add    rax,rdx
 17e:	b9 10 00 00 00       	mov    ecx,0x10
 183:	ba 00 00 00 00       	mov    edx,0x0
 188:	48 f7 f1             	div    rcx
 18b:	48 6b c0 10          	imul   rax,rax,0x10
 18f:	48 29 c4             	sub    rsp,rax
 192:	48 89 e0             	mov    rax,rsp
 195:	48 83 c0 03          	add    rax,0x3
 199:	48 c1 e8 02          	shr    rax,0x2
 19d:	48 c1 e0 02          	shl    rax,0x2
 1a1:	48 89 45 a0          	mov    QWORD PTR [rbp-0x60],rax
 1a5:	8b 45 94             	mov    eax,DWORD PTR [rbp-0x6c]
 1a8:	48 98                	cdqe   
 1aa:	48 83 e8 01          	sub    rax,0x1
 1ae:	48 89 45 a8          	mov    QWORD PTR [rbp-0x58],rax
 1b2:	8b 45 94             	mov    eax,DWORD PTR [rbp-0x6c]
 1b5:	48 98                	cdqe   
 1b7:	49 89 c4             	mov    r12,rax
 1ba:	41 bd 00 00 00 00    	mov    r13d,0x0
 1c0:	8b 45 94             	mov    eax,DWORD PTR [rbp-0x6c]
 1c3:	48 98                	cdqe   
 1c5:	49 89 c2             	mov    r10,rax
 1c8:	41 bb 00 00 00 00    	mov    r11d,0x0
 1ce:	8b 45 94             	mov    eax,DWORD PTR [rbp-0x6c]
 1d1:	48 98                	cdqe   
 1d3:	48 c1 e0 02          	shl    rax,0x2
 1d7:	48 8d 50 03          	lea    rdx,[rax+0x3]
 1db:	b8 10 00 00 00       	mov    eax,0x10
 1e0:	48 83 e8 01          	sub    rax,0x1
 1e4:	48 01 d0             	add    rax,rdx
 1e7:	b9 10 00 00 00       	mov    ecx,0x10
 1ec:	ba 00 00 00 00       	mov    edx,0x0
 1f1:	48 f7 f1             	div    rcx
 1f4:	48 6b c0 10          	imul   rax,rax,0x10
 1f8:	48 29 c4             	sub    rsp,rax
 1fb:	48 89 e0             	mov    rax,rsp
 1fe:	48 83 c0 03          	add    rax,0x3
 202:	48 c1 e8 02          	shr    rax,0x2
 206:	48 c1 e0 02          	shl    rax,0x2
 20a:	48 89 45 b0          	mov    QWORD PTR [rbp-0x50],rax
 20e:	8b 45 94             	mov    eax,DWORD PTR [rbp-0x6c]
 211:	48 98                	cdqe   
 213:	48 83 e8 01          	sub    rax,0x1
 217:	48 89 45 b8          	mov    QWORD PTR [rbp-0x48],rax
 21b:	8b 45 94             	mov    eax,DWORD PTR [rbp-0x6c]
 21e:	48 98                	cdqe   
 220:	49 89 c0             	mov    r8,rax
 223:	41 b9 00 00 00 00    	mov    r9d,0x0
 229:	8b 45 94             	mov    eax,DWORD PTR [rbp-0x6c]
 22c:	48 98                	cdqe   
 22e:	48 89 c6             	mov    rsi,rax
 231:	bf 00 00 00 00       	mov    edi,0x0
 236:	8b 45 94             	mov    eax,DWORD PTR [rbp-0x6c]
 239:	48 98                	cdqe   
 23b:	48 c1 e0 02          	shl    rax,0x2
 23f:	48 8d 50 03          	lea    rdx,[rax+0x3]
 243:	b8 10 00 00 00       	mov    eax,0x10
 248:	48 83 e8 01          	sub    rax,0x1
 24c:	48 01 d0             	add    rax,rdx
 24f:	bf 10 00 00 00       	mov    edi,0x10
 254:	ba 00 00 00 00       	mov    edx,0x0
 259:	48 f7 f7             	div    rdi
 25c:	48 6b c0 10          	imul   rax,rax,0x10
 260:	48 29 c4             	sub    rsp,rax
 263:	48 89 e0             	mov    rax,rsp
 266:	48 83 c0 03          	add    rax,0x3
 26a:	48 c1 e8 02          	shr    rax,0x2
 26e:	48 c1 e0 02          	shl    rax,0x2
 272:	48 89 45 c0          	mov    QWORD PTR [rbp-0x40],rax
  for(int i=0; i < n; i++){
 276:	c7 45 90 00 00 00 00 	mov    DWORD PTR [rbp-0x70],0x0
 27d:	eb 29                	jmp    2a8 <main+0x198>
    foo[i] = i;
 27f:	48 8b 45 a0          	mov    rax,QWORD PTR [rbp-0x60]
 283:	8b 55 90             	mov    edx,DWORD PTR [rbp-0x70]
 286:	48 63 d2             	movsxd rdx,edx
 289:	8b 4d 90             	mov    ecx,DWORD PTR [rbp-0x70]
 28c:	89 0c 90             	mov    DWORD PTR [rax+rdx*4],ecx
    bar[i] = n - i;
 28f:	8b 45 94             	mov    eax,DWORD PTR [rbp-0x6c]
 292:	2b 45 90             	sub    eax,DWORD PTR [rbp-0x70]
 295:	89 c1                	mov    ecx,eax
 297:	48 8b 45 b0          	mov    rax,QWORD PTR [rbp-0x50]
 29b:	8b 55 90             	mov    edx,DWORD PTR [rbp-0x70]
 29e:	48 63 d2             	movsxd rdx,edx
 2a1:	89 0c 90             	mov    DWORD PTR [rax+rdx*4],ecx


int main(void){
  const int n = 10000;
  int32_t foo[n], bar[n], res[n];
  for(int i=0; i < n; i++){
 2a4:	83 45 90 01          	add    DWORD PTR [rbp-0x70],0x1
 2a8:	8b 45 90             	mov    eax,DWORD PTR [rbp-0x70]
 2ab:	3b 45 94             	cmp    eax,DWORD PTR [rbp-0x6c]
 2ae:	7c cf                	jl     27f <main+0x16f>
    foo[i] = i;
    bar[i] = n - i;
  }
  add_i32_simple(res, foo, bar, n);
 2b0:	48 8b 55 b0          	mov    rdx,QWORD PTR [rbp-0x50]
 2b4:	48 8b 75 a0          	mov    rsi,QWORD PTR [rbp-0x60]
 2b8:	48 8b 45 c0          	mov    rax,QWORD PTR [rbp-0x40]
 2bc:	8b 4d 94             	mov    ecx,DWORD PTR [rbp-0x6c]
 2bf:	48 89 c7             	mov    rdi,rax
 2c2:	e8 00 00 00 00       	call   2c7 <main+0x1b7>
 2c7:	48 89 dc             	mov    rsp,rbx
 2ca:	b8 00 00 00 00       	mov    eax,0x0
}
 2cf:	48 8b 7d c8          	mov    rdi,QWORD PTR [rbp-0x38]
 2d3:	64 48 33 3c 25 28 00 	xor    rdi,QWORD PTR fs:0x28
 2da:	00 00 
 2dc:	74 05                	je     2e3 <main+0x1d3>
 2de:	e8 00 00 00 00       	call   2e3 <main+0x1d3>
 2e3:	48 8d 65 d8          	lea    rsp,[rbp-0x28]
 2e7:	5b                   	pop    rbx
 2e8:	41 5c                	pop    r12
 2ea:	41 5d                	pop    r13
 2ec:	41 5e                	pop    r14
 2ee:	41 5f                	pop    r15
 2f0:	5d                   	pop    rbp
 2f1:	c3                   	ret    
