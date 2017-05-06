; ModuleID = 'Grail'

@fmt = private unnamed_addr constant [4 x i8] c"%d\0A\00"
@fmt1 = private unnamed_addr constant [4 x i8] c"%f\0A\00"
@str = private unnamed_addr constant [5 x i8] c"IN F\00"
@fmt2 = private unnamed_addr constant [4 x i8] c"%d\0A\00"
@fmt3 = private unnamed_addr constant [4 x i8] c"%f\0A\00"
@str4 = private unnamed_addr constant [5 x i8] c"MAIN\00"

declare i32 @printf(i8*, ...)

define void @f() {
entry:
  %printf = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([5 x i8]* @str, i32 0, i32 0))
  ret void
}

define void @main() {
entry:
  call void @f()
  %printf = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([5 x i8]* @str4, i32 0, i32 0))
  ret void
}
