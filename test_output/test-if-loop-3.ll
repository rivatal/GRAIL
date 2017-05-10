; ModuleID = 'Grail'

@fmt = private unnamed_addr constant [4 x i8] c"%d\0A\00"
@fmt1 = private unnamed_addr constant [4 x i8] c"%f\0A\00"
@fmt2 = private unnamed_addr constant [4 x i8] c"%d\0A\00"
@fmt3 = private unnamed_addr constant [4 x i8] c"%f\0A\00"
@fmt4 = private unnamed_addr constant [4 x i8] c"%d\0A\00"
@fmt5 = private unnamed_addr constant [4 x i8] c"%f\0A\00"
@fmt6 = private unnamed_addr constant [4 x i8] c"%d\0A\00"
@fmt7 = private unnamed_addr constant [4 x i8] c"%f\0A\00"
@fmt8 = private unnamed_addr constant [4 x i8] c"%d\0A\00"
@fmt9 = private unnamed_addr constant [4 x i8] c"%f\0A\00"
@str = private unnamed_addr constant [7 x i8] c"Bigger\00"
@str10 = private unnamed_addr constant [6 x i8] c"Equal\00"

declare i32 @printf(i8*, ...)

declare i32 @sample_display(i32)

define void @"print!1"(i8* %x) {
entry:
  %x1 = alloca i8*
  store i8* %x, i8** %x1
  ret void
}

define void @"print!2"(i8* %x) {
entry:
  %x1 = alloca i8*
  store i8* %x, i8** %x1
  ret void
}

define void @"print!3"(i8* %x) {
entry:
  %x1 = alloca i8*
  store i8* %x, i8** %x1
  ret void
}

define void @"print!4"(i8* %x) {
entry:
  %x1 = alloca i8*
  store i8* %x, i8** %x1
  ret void
}

define i32 @main() {
entry:
  %a = alloca i32
  store i32 5, i32* %a
  %a1 = load i32* %a
  %tmp = icmp slt i32 %a1, 3
  br i1 %tmp, label %then, label %else

merge:                                            ; preds = %merge4, %then
  ret i32 0

then:                                             ; preds = %entry
  %printf = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([7 x i8]* @str, i32 0, i32 0))
  br label %merge

else:                                             ; preds = %entry
  %a2 = load i32* %a
  %tmp3 = icmp eq i32 %a2, 5
  br i1 %tmp3, label %then5, label %else7

merge4:                                           ; preds = %else7, %then5
  br label %merge

then5:                                            ; preds = %else
  %printf6 = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([6 x i8]* @str10, i32 0, i32 0))
  br label %merge4

else7:                                            ; preds = %else
  br label %merge4
}
