updating map func for (a: int) = (5: int); 
matching x with type: strassigning x to strupdating map func for  (call print((Bigger: str))) : void; 
matching x with type: strupdating map func for if (((a: int) > (0: int): bool)) { (call print((Bigger: str))) : void; 
; 
updating map func for  (call print((Bigger: str))) : void; 
matching x with type: str; ModuleID = 'Grail'

@fmt = private unnamed_addr constant [4 x i8] c"%d\0A\00"
@fmt1 = private unnamed_addr constant [4 x i8] c"%f\0A\00"
@fmt2 = private unnamed_addr constant [4 x i8] c"%d\0A\00"
@fmt3 = private unnamed_addr constant [4 x i8] c"%f\0A\00"
@str = private unnamed_addr constant [7 x i8] c"Bigger\00"

declare i32 @printf(i8*, ...)

declare i32 @sample_display(i32)

define void @"print!1"(i8* %x) {
entry:
  %x1 = alloca i8*
  store i8* %x, i8** %x1
  ret void
}

define void @main() {
entry:
  %a = alloca i32
  store i32 5, i32* %a
  %a1 = load i32* %a
  %tmp = icmp sgt i32 %a1, 0
  br i1 %tmp, label %then, label %else

merge:                                            ; preds = %else, %then
  ret void

then:                                             ; preds = %entry
  %printf = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([7 x i8]* @str, i32 0, i32 0))
  br label %merge

else:                                             ; preds = %entry
  br label %merge
}
