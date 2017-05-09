; ModuleID = 'Grail'

@fmt = private unnamed_addr constant [4 x i8] c"%d\0A\00"
@fmt1 = private unnamed_addr constant [4 x i8] c"%f\0A\00"
@fmt2 = private unnamed_addr constant [4 x i8] c"%d\0A\00"
@fmt3 = private unnamed_addr constant [4 x i8] c"%f\0A\00"
@str = private unnamed_addr constant [5 x i8] c"IN F\00"
@fmt4 = private unnamed_addr constant [4 x i8] c"%d\0A\00"
@fmt5 = private unnamed_addr constant [4 x i8] c"%f\0A\00"
@str6 = private unnamed_addr constant [5 x i8] c"IN F\00"
@fmt7 = private unnamed_addr constant [4 x i8] c"%d\0A\00"
@fmt8 = private unnamed_addr constant [4 x i8] c"%f\0A\00"
@fmt9 = private unnamed_addr constant [4 x i8] c"%d\0A\00"
@fmt10 = private unnamed_addr constant [4 x i8] c"%f\0A\00"
@str11 = private unnamed_addr constant [5 x i8] c"MAIN\00"

declare i32 @printf(i8*, ...)

declare i32 @sample_display(i32)

define void @"print!1"(i8* %x) {
entry:
  %x1 = alloca i8*
  store i8* %x, i8** %x1
  ret void
}

define void @f() {
entry:
  %printf = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([5 x i8]* @str, i32 0, i32 0))
  ret void
}

define void @"f!3"() {
entry:
  %printf = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([5 x i8]* @str6, i32 0, i32 0))
  ret void
}

define void @"print!4"(i8* %x) {
entry:
  %x1 = alloca i8*
  store i8* %x, i8** %x1
  ret void
}

define void @main() {
entry:
  call void @"f!3"()
  %printf = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([5 x i8]* @str11, i32 0, i32 0))
  ret void
}
