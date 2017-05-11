; ModuleID = 'Grail'

@fmt = private unnamed_addr constant [4 x i8] c"%d\0A\00"
@fmt1 = private unnamed_addr constant [4 x i8] c"%f\0A\00"
@fmt2 = private unnamed_addr constant [4 x i8] c"%d\0A\00"
@fmt3 = private unnamed_addr constant [4 x i8] c"%f\0A\00"
@fmt4 = private unnamed_addr constant [4 x i8] c"%d\0A\00"
@fmt5 = private unnamed_addr constant [4 x i8] c"%f\0A\00"
@str = private unnamed_addr constant [4 x i8] c"sdf\00"

declare i32 @printf(i8*, ...)

declare i32 @sample_display(i32)

define i32 @"f!1"(i32 %x, i8* %y) {
entry:
  %x1 = alloca i32
  store i32 %x, i32* %x1
  %y2 = alloca i8*
  store i8* %y, i8** %y2
  %x3 = load i32* %x1
  ret i32 %x3
}

define i32 @"f!2"(i32 %x, i8* %y) {
entry:
  %x1 = alloca i32
  store i32 %x, i32* %x1
  %y2 = alloca i8*
  store i8* %y, i8** %y2
  %x3 = load i32* %x1
  ret i32 %x3
}

define i32 @main() {
entry:
  %"f!2_result" = call i32 @"f!2"(i32 3, i8* getelementptr inbounds ([4 x i8]* @str, i32 0, i32 0))
  ret i32 0
}
