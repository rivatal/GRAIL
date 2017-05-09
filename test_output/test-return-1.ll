updating map func for (i: str) = (hello world: str); 
updating map func for return (0: int); int

; ModuleID = 'Grail'

@fmt = private unnamed_addr constant [4 x i8] c"%d\0A\00"
@fmt1 = private unnamed_addr constant [4 x i8] c"%f\0A\00"
@str = private unnamed_addr constant [12 x i8] c"hello world\00"

declare i32 @printf(i8*, ...)

declare i32 @sample_display(i32)

define i32 @main() {
entry:
  %i = alloca i8*
  store i8* getelementptr inbounds ([12 x i8]* @str, i32 0, i32 0), i8** %i
  ret i32 0
}
