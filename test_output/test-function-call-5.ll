updating map func for (c: int) = ((a: int) + (b: int): int); 
updating map func for return (c: int); int

matching a with type: intassigning a to intmatching b with type: intassigning b to intupdating map func for (c: int) = ((a: int) + (b: int): int); 
updating map func for return (c: int); int

updating map func for (d: int) = (call add((52: int),(10: int))) : int; 
matching a with type: intmatching b with type: intmatching x with type: intassigning x to intupdating map func for  (call printint((d: int))) : void; 
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

define i32 @"add!1"(i32 %a, i32 %b) {
entry:
  %a1 = alloca i32
  store i32 %a, i32* %a1
  %b2 = alloca i32
  store i32 %b, i32* %b2
  %a3 = load i32* %a1
  %b4 = load i32* %b2
  %tmp = add i32 %a3, %b4
  %c = alloca i32
  store i32 %tmp, i32* %c
  %c5 = load i32* %c
  ret i32 %c5
}

define void @"printint!2"(i32 %x) {
entry:
  %x1 = alloca i32
  store i32 %x, i32* %x1
  ret void
}

define i32 @main() {
entry:
  %"add!1_result" = call i32 @"add!1"(i32 52, i32 10)
  %d = alloca i32
  store i32 %"add!1_result", i32* %d
  %d1 = load i32* %d
  %printf = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([4 x i8]* @fmt4, i32 0, i32 0), i32 %d1)
  ret i32 0
}
