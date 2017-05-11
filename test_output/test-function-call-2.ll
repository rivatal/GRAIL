; ModuleID = 'Grail'

@fmt = private unnamed_addr constant [4 x i8] c"%d\0A\00"
@fmt1 = private unnamed_addr constant [4 x i8] c"%f\0A\00"
@str = private unnamed_addr constant [5 x i8] c"IN F\00"
@fmt2 = private unnamed_addr constant [4 x i8] c"%d\0A\00"
@fmt3 = private unnamed_addr constant [4 x i8] c"%f\0A\00"
@str4 = private unnamed_addr constant [5 x i8] c"IN F\00"
@fmt5 = private unnamed_addr constant [4 x i8] c"%d\0A\00"
@fmt6 = private unnamed_addr constant [4 x i8] c"%f\0A\00"
@str7 = private unnamed_addr constant [5 x i8] c"IN F\00"
@fmt8 = private unnamed_addr constant [4 x i8] c"%d\0A\00"
@fmt9 = private unnamed_addr constant [4 x i8] c"%f\0A\00"
@str10 = private unnamed_addr constant [5 x i8] c"MAIN\00"

declare i32 @printf(i8*, ...)

declare i32 @sample_display(i32)

define void @f() {
entry:
  %printf = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([5 x i8]* @str, i32 0, i32 0))
  ret void
}

define void @"f!4"() {
entry:
  %printf = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([5 x i8]* @str4, i32 0, i32 0))
  ret void
}

define void @"f!7"() {
entry:
  %printf = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([5 x i8]* @str7, i32 0, i32 0))
  ret void
}

define i32 @main() {
entry:
  call void @"f!7"()
  %printf = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([5 x i8]* @str10, i32 0, i32 0))
  ret i32 0
}
