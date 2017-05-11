; ModuleID = 'Grail'

@fmt = private unnamed_addr constant [4 x i8] c"%d\0A\00"
@fmt1 = private unnamed_addr constant [4 x i8] c"%f\0A\00"
@fmt2 = private unnamed_addr constant [4 x i8] c"%d\0A\00"
@fmt3 = private unnamed_addr constant [4 x i8] c"%f\0A\00"
@fmt4 = private unnamed_addr constant [4 x i8] c"%d\0A\00"
@fmt5 = private unnamed_addr constant [4 x i8] c"%f\0A\00"

declare i32 @printf(i8*, ...)

declare i32 @sample_display(i32)

define i32 @"add!1"(i32 %x) {
entry:
  %x1 = alloca i32
  store i32 %x, i32* %x1
  %x2 = load i32* %x1
  %tmp = add i32 %x2, 1
  ret i32 %tmp
}

define i32 @"add!3"(i32 %x) {
entry:
  %x1 = alloca i32
  store i32 %x, i32* %x1
  %x2 = load i32* %x1
  %tmp = add i32 %x2, 1
  ret i32 %tmp
}

define i32 @main() {
entry:
  %x = alloca i32
  store i32 5, i32* %x
  %x1 = load i32* %x
  %"add!3_result" = call i32 @"add!3"(i32 %x1)
  %y = alloca i32
  store i32 %"add!3_result", i32* %y
  %y2 = load i32* %y
  %printf = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([4 x i8]* @fmt4, i32 0, i32 0), i32 %y2)
  ret i32 0
}
