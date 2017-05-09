updating map func for (j: int) = (0: int); 
updating map func for (j: int) = ((j: int) + (2: int): int); 
updating map func for (a: int) = ((a: int) - (1: int): int); 
updating map func for while (((a: int) > (0: int): bool)) {(j: int) = ((j: int) + (2: int): int); 
(a: int) = ((a: int) - (1: int): int); 
}
updating map func for return (j: int); int

matching a with type: intassigning a to intupdating map func for (j: int) = (0: int); 
updating map func for (j: int) = ((j: int) + (2: int): int); 
updating map func for (a: int) = ((a: int) - (1: int): int); 
updating map func for while (((a: int) > (0: int): bool)) {(j: int) = ((j: int) + (2: int): int); 
(a: int) = ((a: int) - (1: int): int); 
}
updating map func for return (j: int); int

updating map func for (b: int) = (call foo((7: int))) : int; 
matching a with type: intmatching x with type: intassigning x to intupdating map func for  (call printint((b: int))) : void; 
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

define i32 @"foo!1"(i32 %a) {
entry:
  %a1 = alloca i32
  store i32 %a, i32* %a1
  %j = alloca i32
  store i32 0, i32* %j
  br label %while

while:                                            ; preds = %while_body, %entry
  %a5 = load i32* %a1
  %tmp6 = icmp sgt i32 %a5, 0
  br i1 %tmp6, label %while_body, label %merge

while_body:                                       ; preds = %while
  %j2 = load i32* %j
  %tmp = add i32 %j2, 2
  store i32 %tmp, i32* %j
  %a3 = load i32* %a1
  %tmp4 = sub i32 %a3, 1
  store i32 %tmp4, i32* %a1
  br label %while

merge:                                            ; preds = %while
  %j7 = load i32* %j
  ret i32 %j7
}

define void @"printint!2"(i32 %x) {
entry:
  %x1 = alloca i32
  store i32 %x, i32* %x1
  ret void
}

define i32 @main() {
entry:
  %"foo!1_result" = call i32 @"foo!1"(i32 7)
  %b = alloca i32
  store i32 %"foo!1_result", i32* %b
  %b1 = load i32* %b
  %printf = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([4 x i8]* @fmt4, i32 0, i32 0), i32 %b1)
  ret i32 0
}
