updating map func for (a: float) = (1.1: float); 
updating map func for (b: float) = (2.2: float); 
updating map func for (a: float) = ((a: float) .+ (b: float): float); 
updating map func for (b: float) = ((a: float) .- (b: float): float); 
updating map func for (c: float) = (((a: float) .* (a: float): float) .* (b: float): float); 
updating map func for (d: float) = ((b: float) ./ (a: float): float); 
updating map func for return (0: int); int

; ModuleID = 'Grail'

@fmt = private unnamed_addr constant [4 x i8] c"%d\0A\00"
@fmt1 = private unnamed_addr constant [4 x i8] c"%f\0A\00"

declare i32 @printf(i8*, ...)

declare i32 @sample_display(i32)

define i32 @main() {
entry:
  %a = alloca float
  store float 0x3FF19999A0000000, float* %a
  %b = alloca float
  store float 0x40019999A0000000, float* %b
  %a1 = load float* %a
  %b2 = load float* %b
  %tmp = fadd float %a1, %b2
  store float %tmp, float* %a
  %a3 = load float* %a
  %b4 = load float* %b
  %tmp5 = fsub float %a3, %b4
  store float %tmp5, float* %b
  %a6 = load float* %a
  %a7 = load float* %a
  %tmp8 = fmul float %a6, %a7
  %b9 = load float* %b
  %tmp10 = fmul float %tmp8, %b9
  %c = alloca float
  store float %tmp10, float* %c
  %b11 = load float* %b
  %a12 = load float* %a
  %tmp13 = fdiv float %b11, %a12
  %d = alloca float
  store float %tmp13, float* %d
  ret i32 0
}
