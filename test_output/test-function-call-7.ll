updating map func for (d: int) = ((a: int) + (c: int): int); 
updating map func for return (d: int); int

matching a with type: intassigning a to intmatching b with type: boolassigning b to boolmatching c with type: intassigning c to intupdating map func for (d: int) = ((a: int) + (c: int): int); 
updating map func for return (d: int); int

updating map func for (f: int) = (call bar((17: int),(false: bool),(25: int))) : int; 
matching a with type: intmatching b with type: boolmatching c with type: intmatching x with type: intassigning x to intupdating map func for  (call printint((f: int))) : void; 
matching x with type: intupdating map func for return (0: int); int

; ModuleID = 'Grail'

@fmt = private unnamed_addr constant [4 x i8] c"%d\0A\00"
@fmt1 = private unnamed_addr constant [4 x i8] c"%f\0A\00"
@fmt2 = private unnamed_addr constant [4 x i8] c"%d\0A\00"
@fmt3 = private unnamed_addr constant [4 x i8] c"%f\0A\00"
@fmt4 = private unnamed_addr constant [4 x i8] c"%d\0A\00"
@fmt5 = private unnamed_addr constant [4 x i8] c"%f\0A\00"
@fmt6 = private unnamed_addr constant [4 x i8] c"%d\0A\00"
@fmt7 = private unnamed_addr constant [4 x i8] c"%f\0A\00"

declare i32 @printf(i8*, ...)

declare i32 @sample_display(i32)

define void @foo() {
entry:
  ret void
}

define i32 @"bar!1"(i32 %a, i1 %b, i32 %c) {
entry:
  %a1 = alloca i32
  store i32 %a, i32* %a1
  %b2 = alloca i1
  store i1 %b, i1* %b2
  %c3 = alloca i32
  store i32 %c, i32* %c3
  %a4 = load i32* %a1
  %c5 = load i32* %c3
  %tmp = add i32 %a4, %c5
  %d = alloca i32
  store i32 %tmp, i32* %d
  %d6 = load i32* %d
  ret i32 %d6
}

define void @"printint!2"(i32 %x) {
entry:
  %x1 = alloca i32
  store i32 %x, i32* %x1
  ret void
}

define i32 @main() {
entry:
  %"bar!1_result" = call i32 @"bar!1"(i32 17, i1 false, i32 25)
  %f = alloca i32
  store i32 %"bar!1_result", i32* %f
  %f1 = load i32* %f
  %printf = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([4 x i8]* @fmt6, i32 0, i32 0), i32 %f1)
  ret i32 0
}
