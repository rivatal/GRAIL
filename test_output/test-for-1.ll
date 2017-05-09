updating map func for (i: int) = (0: int); 
updating map func for (i: int) = (0: int); 
updating map func for (i: int) = ((i: int) + (1: int): int); 
matching x with type: intassigning x to intupdating map func for  (call printint((i: int))) : void; 
matching x with type: intupdating map func for for ((i: int) = (0: int); 
((i: int) < (5: int): bool) ; (i: int) = ((i: int) + (1: int): int); 
 (call printint((i: int))) : void; 

updating map func for (i: int) = (0: int); 
updating map func for (i: int) = ((i: int) + (1: int): int); 
updating map func for  (call printint((i: int))) : void; 
matching x with type: intmatching x with type: intassigning x to intupdating map func for  (call printint((42: int))) : void; 
matching x with type: intupdating map func for return (0: int); int

; ModuleID = 'Grail'

@fmt = private unnamed_addr constant [4 x i8] c"%d\0A\00"
@fmt1 = private unnamed_addr constant [4 x i8] c"%f\0A\00"
@fmt2 = private unnamed_addr constant [4 x i8] c"%d\0A\00"
@fmt3 = private unnamed_addr constant [4 x i8] c"%f\0A\00"
@fmt4 = private unnamed_addr constant [4 x i8] c"%d\0A\00"
@fmt5 = private unnamed_addr constant [4 x i8] c"%f\0A\00"

declare i32 @printf(i8*, ...)

declare i32 @sample_display(i32)

define void @"printint!1"(i32 %x) {
entry:
  %x1 = alloca i32
  store i32 %x, i32* %x1
  ret void
}

define void @"printint!2"(i32 %x) {
entry:
  %x1 = alloca i32
  store i32 %x, i32* %x1
  ret void
}

define i32 @main() {
entry:
  %i = alloca i32
  store i32 0, i32* %i
  store i32 0, i32* %i
  br label %while

while:                                            ; preds = %while_body, %entry
  %i3 = load i32* %i
  %tmp4 = icmp slt i32 %i3, 5
  br i1 %tmp4, label %while_body, label %merge

while_body:                                       ; preds = %while
  %i1 = load i32* %i
  %printf = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([4 x i8]* @fmt4, i32 0, i32 0), i32 %i1)
  %i2 = load i32* %i
  %tmp = add i32 %i2, 1
  store i32 %tmp, i32* %i
  br label %while

merge:                                            ; preds = %while
  %printf5 = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([4 x i8]* @fmt4, i32 0, i32 0), i32 42)
  ret i32 0
}
