updating map func for (a: int) = ((a: int) + (1: int): int); 
updating map func for return (a: int); int

matching a with type: intassigning a to intupdating map func for (a: int) = ((a: int) + (1: int): int); 
updating map func for return (a: int); int

updating map func for (x: int) = (call f((3: int))) : int; 
matching a with type: intmatching x with type: intassigning x to intupdating map func for  (call printint((x: int))) : void; 
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

define i32 @"f!1"(i32 %a) {
entry:
  %a1 = alloca i32
  store i32 %a, i32* %a1
  %a2 = load i32* %a1
  %tmp = add i32 %a2, 1
  store i32 %tmp, i32* %a1
  %a3 = load i32* %a1
  ret i32 %a3
}

define void @"printint!2"(i32 %x) {
entry:
  %x1 = alloca i32
  store i32 %x, i32* %x1
  ret void
}

define i32 @main() {
entry:
  %"f!1_result" = call i32 @"f!1"(i32 3)
  %x = alloca i32
  store i32 %"f!1_result", i32* %x
  %x1 = load i32* %x
  %printf = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([4 x i8]* @fmt4, i32 0, i32 0), i32 %x1)
  ret i32 0
}
