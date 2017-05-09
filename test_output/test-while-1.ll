updating map func for (i: int) = (5: int); 
matching x with type: intassigning x to intupdating map func for  (call printint((i: int))) : void; 
matching x with type: intupdating map func for (i: int) = ((i: int) - (1: int): int); 
updating map func for while (((i: int) > (0: int): bool)) { (call printint((i: int))) : void; 
(i: int) = ((i: int) - (1: int): int); 
}
matching x with type: strassigning x to strupdating map func for  (call print((42: str))) : void; 
matching x with type: strupdating map func for return (0: int); int

; ModuleID = 'Grail'

@fmt = private unnamed_addr constant [4 x i8] c"%d\0A\00"
@fmt1 = private unnamed_addr constant [4 x i8] c"%f\0A\00"
@fmt2 = private unnamed_addr constant [4 x i8] c"%d\0A\00"
@fmt3 = private unnamed_addr constant [4 x i8] c"%f\0A\00"
@str = private unnamed_addr constant [3 x i8] c"42\00"

declare i32 @printf(i8*, ...)

declare i32 @sample_display(i32)

define void @"print!2"(i8* %x) {
entry:
  %x1 = alloca i8*
  store i8* %x, i8** %x1
  ret void
}

define i32 @main() {
entry:
  %i = alloca i32
  store i32 5, i32* %i
  br label %while

while:                                            ; preds = %while_body, %entry
  %i3 = load i32* %i
  %tmp4 = icmp sgt i32 %i3, 0
  br i1 %tmp4, label %while_body, label %merge

while_body:                                       ; preds = %while
  %i1 = load i32* %i
  %printf = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([4 x i8]* @fmt2, i32 0, i32 0), i32 %i1)
  %i2 = load i32* %i
  %tmp = sub i32 %i2, 1
  store i32 %tmp, i32* %i
  br label %while

merge:                                            ; preds = %while
  %printf5 = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([3 x i8]* @str, i32 0, i32 0))
  ret i32 0
}
