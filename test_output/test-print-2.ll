; ModuleID = 'Grail'

@fmt = private unnamed_addr constant [4 x i8] c"%d\0A\00"
@fmt1 = private unnamed_addr constant [4 x i8] c"%f\0A\00"
@fmt2 = private unnamed_addr constant [4 x i8] c"%d\0A\00"
@fmt3 = private unnamed_addr constant [4 x i8] c"%f\0A\00"
@fmt4 = private unnamed_addr constant [4 x i8] c"%d\0A\00"
@fmt5 = private unnamed_addr constant [4 x i8] c"%f\0A\00"

declare i32 @printf(i8*, ...)

declare i32 @sample_display(i32)

define void @"printem!4"(i32 %a) {
entry:
  %a1 = alloca i32
  store i32 %a, i32* %a1
  %a2 = load i32* %a1
  %printf = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([4 x i8]* @fmt, i32 0, i32 0), i32 %a2)
  ret void
}

define void @"printem!6"(i32 %a) {
entry:
  %a1 = alloca i32
  store i32 %a, i32* %a1
  %a2 = load i32* %a1
  %printf = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([4 x i8]* @fmt2, i32 0, i32 0), i32 %a2)
  ret void
}

define i32 @main() {
entry:
  call void @"printem!6"(i32 42)
  ret i32 0
}
