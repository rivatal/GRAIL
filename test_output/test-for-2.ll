updating map func for (a: int) = (5: int); 
updating map func for (a: int) = (5: int); 
updating map func for (a: int) = ((a: int) - (1: int): int); 
matching x with type: boolassigning x to boolupdating map func for  (call printbool((true: bool))) : void; 
matching x with type: boolupdating map func for for ((a: int) = (5: int); 
((a: int) >= (0: int): bool) ; (a: int) = ((a: int) - (1: int): int); 
 (call printbool((true: bool))) : void; 

updating map func for (a: int) = (5: int); 
updating map func for (a: int) = ((a: int) - (1: int): int); 
updating map func for  (call printbool((true: bool))) : void; 
matching x with type: boolmatching x with type: strassigning x to strupdating map func for  (call print((Complete: str))) : void; 
matching x with type: strupdating map func for return (0: int); int

; ModuleID = 'Grail'

@fmt = private unnamed_addr constant [4 x i8] c"%d\0A\00"
@fmt1 = private unnamed_addr constant [4 x i8] c"%f\0A\00"
@fmt2 = private unnamed_addr constant [4 x i8] c"%d\0A\00"
@fmt3 = private unnamed_addr constant [4 x i8] c"%f\0A\00"
@fmt4 = private unnamed_addr constant [4 x i8] c"%d\0A\00"
@fmt5 = private unnamed_addr constant [4 x i8] c"%f\0A\00"
@str = private unnamed_addr constant [9 x i8] c"Complete\00"

declare i32 @printf(i8*, ...)

declare i32 @sample_display(i32)

define void @"printbool!1"(i1 %x) {
entry:
  %x1 = alloca i1
  store i1 %x, i1* %x1
  ret void
}

define void @"print!2"(i8* %x) {
entry:
  %x1 = alloca i8*
  store i8* %x, i8** %x1
  ret void
}

define i32 @main() {
entry:
  %a = alloca i32
  store i32 5, i32* %a
  store i32 5, i32* %a
  br label %while

while:                                            ; preds = %while_body, %entry
  %a2 = load i32* %a
  %tmp3 = icmp sge i32 %a2, 0
  br i1 %tmp3, label %while_body, label %merge

while_body:                                       ; preds = %while
  %printf = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([4 x i8]* @fmt4, i32 0, i32 0), i1 true)
  %a1 = load i32* %a
  %tmp = sub i32 %a1, 1
  store i32 %tmp, i32* %a
  br label %while

merge:                                            ; preds = %while
  %printf4 = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([9 x i8]* @str, i32 0, i32 0))
  ret i32 0
}
