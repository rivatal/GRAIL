; ModuleID = 'Grail'

@str = private unnamed_addr constant [12 x i8] c"hello world\00"

declare i32 @printf(i8*, ...)

define void @main() {
entry:
  %printf = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([12 x i8]* @str, i32 0, i32 0))
  ret void
}
