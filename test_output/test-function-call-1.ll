; ModuleID = 'Grail'

@fmt = private unnamed_addr constant [4 x i8] c"%d\0A\00"
@fmt1 = private unnamed_addr constant [4 x i8] c"%f\0A\00"
@str = private unnamed_addr constant [5 x i8] c"IN Y\00"

declare i32 @printf(i8*, ...)

declare i32 @sample_display(i32)

define i32 @main() {
entry:
  %x = alloca i32
  store i32 3, i32* %x
  %printf = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([5 x i8]* @str, i32 0, i32 0))
  ret i32 0
}