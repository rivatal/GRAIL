; ModuleID = 'Grail'

@fmt = private unnamed_addr constant [4 x i8] c"%d\0A\00"
@fmt.1 = private unnamed_addr constant [4 x i8] c"%f\0A\00"
@fmt.2 = private unnamed_addr constant [4 x i8] c"%d\0A\00"
@fmt.3 = private unnamed_addr constant [4 x i8] c"%f\0A\00"
@fmt.4 = private unnamed_addr constant [4 x i8] c"%d\0A\00"
@fmt.5 = private unnamed_addr constant [4 x i8] c"%f\0A\00"

declare i32 @printf(i8*, ...)

declare i32 @sample_display(i32)

define i32 @"gcd!1"(i32 %b, i32 %a) {
entry:
  %b1 = alloca i32
  store i32 %b, i32* %b1
  %a2 = alloca i32
  store i32 %a, i32* %a2
  %a3 = load i32, i32* %a2
  ret i32 %a3
}

define void @"printint!2"(i32 %x) {
entry:
  %x1 = alloca i32
  store i32 %x, i32* %x1
  ret void
}

define i32 @main() {
entry:
  %"gcd!1_result" = call i32 @"gcd!1"(i32 2, i32 14)
  %c = alloca i32
  store i32 %"gcd!1_result", i32* %c
  %c1 = load i32, i32* %c
  %printf = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @fmt.4, i32 0, i32 0), i32 %c1)
  ret i32 0
}
