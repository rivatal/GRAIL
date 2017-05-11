; ModuleID = 'Grail'

@fmt = private unnamed_addr constant [4 x i8] c"%d\0A\00"
@fmt1 = private unnamed_addr constant [4 x i8] c"%f\0A\00"
@str = private unnamed_addr constant [7 x i8] c"Bigger\00"

declare i32 @printf(i8*, ...)

declare i32 @sample_display(i32)

define i32 @main() {
entry:
  %a = alloca i32
  store i32 5, i32* %a
  %a1 = load i32* %a
  %tmp = icmp sgt i32 %a1, 0
  br i1 %tmp, label %then, label %else

merge:                                            ; preds = %else, %then
  ret i32 0

then:                                             ; preds = %entry
  %printf = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([7 x i8]* @str, i32 0, i32 0))
  br label %merge

else:                                             ; preds = %entry
  br label %merge
}
