; ModuleID = 'Grail'

@fmt = private unnamed_addr constant [4 x i8] c"%d\0A\00"
@fmt1 = private unnamed_addr constant [4 x i8] c"%f\0A\00"
@fmt2 = private unnamed_addr constant [4 x i8] c"%d\0A\00"
@fmt3 = private unnamed_addr constant [4 x i8] c"%f\0A\00"
@fmt4 = private unnamed_addr constant [4 x i8] c"%d\0A\00"
@fmt5 = private unnamed_addr constant [4 x i8] c"%f\0A\00"
@fmt6 = private unnamed_addr constant [4 x i8] c"%d\0A\00"
@fmt7 = private unnamed_addr constant [4 x i8] c"%f\0A\00"
@fmt8 = private unnamed_addr constant [4 x i8] c"%d\0A\00"
@fmt9 = private unnamed_addr constant [4 x i8] c"%f\0A\00"
@fmt10 = private unnamed_addr constant [4 x i8] c"%d\0A\00"
@fmt11 = private unnamed_addr constant [4 x i8] c"%f\0A\00"
@fmt12 = private unnamed_addr constant [4 x i8] c"%d\0A\00"
@fmt13 = private unnamed_addr constant [4 x i8] c"%f\0A\00"
@str = private unnamed_addr constant [4 x i8] c"cat\00"
@str14 = private unnamed_addr constant [5 x i8] c"bird\00"
@str15 = private unnamed_addr constant [4 x i8] c"dog\00"
@str16 = private unnamed_addr constant [4 x i8] c"dog\00"

declare i32 @printf(i8*, ...)

declare i32 @sample_display(i32)

define void @"print!1"(i8* %x) {
entry:
  %x1 = alloca i8*
  store i8* %x, i8** %x1
  ret void
}

define void @"print!2"(i8* %x) {
entry:
  %x1 = alloca i8*
  store i8* %x, i8** %x1
  ret void
}

define void @"print!3"(i8* %x) {
entry:
  %x1 = alloca i8*
  store i8* %x, i8** %x1
  ret void
}

define void @"print!4"(i8* %x) {
entry:
  %x1 = alloca i8*
  store i8* %x, i8** %x1
  ret void
}

define void @"print!5"(i8* %x) {
entry:
  %x1 = alloca i8*
  store i8* %x, i8** %x1
  ret void
}

define void @"print!6"(i8* %x) {
entry:
  %x1 = alloca i8*
  store i8* %x, i8** %x1
  ret void
}

define i32 @main() {
entry:
  %strct = alloca { i8**, i32 }
  %lst = alloca i8*, i32 2
  %ptr = getelementptr inbounds i8** %lst, i32 0
  store i8* getelementptr inbounds ([4 x i8]* @str, i32 0, i32 0), i8** %ptr
  %ptr1 = getelementptr inbounds i8** %lst, i32 1
  store i8* getelementptr inbounds ([5 x i8]* @str14, i32 0, i32 0), i8** %ptr1
  %p0 = getelementptr inbounds { i8**, i32 }* %strct, i32 0, i32 0
  %p1 = getelementptr inbounds { i8**, i32 }* %strct, i32 0, i32 1
  store i8** %lst, i8*** %p0
  store i32 2, i32* %p1
  %lst2 = load { i8**, i32 }* %strct
  %x = alloca { i8**, i32 }
  store { i8**, i32 } %lst2, { i8**, i32 }* %x
  %tmp = getelementptr inbounds { i8**, i32 }* %x, i32 0, i32 0
  %tmpar = load i8*** %tmp
  %ptr3 = getelementptr inbounds i8** %tmpar, i32 0
  %item = load i8** %ptr3
  %printf = call i32 (i8*, ...)* @printf(i8* %item)
  %x4 = load { i8**, i32 }* %x
  %strct5 = alloca { i8**, i32 }
  %strct6 = alloca { i8**, i32 }
  store { i8**, i32 } %x4, { i8**, i32 }* %strct6
  %tmp7 = getelementptr inbounds { i8**, i32 }* %strct6, i32 0, i32 1
  %len = load i32* %tmp7
  %tmp8 = getelementptr inbounds { i8**, i32 }* %strct6, i32 0, i32 0
  %lst9 = load i8*** %tmp8
  %tmp10 = getelementptr inbounds { i8**, i32 }* %strct5, i32 0, i32 1
  store i32 %len, i32* %tmp10
  %lst11 = alloca i8*, i32 %len
  %ind = alloca i32
  store i32 0, i32* %ind
  br label %checklimits

checklimits:                                      ; preds = %assignment, %entry
  %tmp16 = load i32* %ind
  %comp = icmp slt i32 %tmp16, %len
  br i1 %comp, label %assignment, label %merge

assignment:                                       ; preds = %checklimits
  %i = load i32* %ind
  %ptr12 = getelementptr inbounds i8** %lst9, i32 %i
  %ptr13 = getelementptr inbounds i8** %lst11, i32 %i
  %tmp14 = load i8** %ptr12
  store i8* %tmp14, i8** %ptr13
  %tmp15 = load i32* %ind
  %inc = add i32 %tmp15, 1
  store i32 %inc, i32* %ind
  br label %checklimits

merge:                                            ; preds = %checklimits
  %tmp17 = getelementptr inbounds { i8**, i32 }* %strct5, i32 0, i32 0
  store i8** %lst11, i8*** %tmp17
  %strct18 = load { i8**, i32 }* %strct5
  %y = alloca { i8**, i32 }
  store { i8**, i32 } %strct18, { i8**, i32 }* %y
  %tmp19 = getelementptr inbounds { i8**, i32 }* %y, i32 0, i32 0
  %tmpar20 = load i8*** %tmp19
  %ptr21 = getelementptr inbounds i8** %tmpar20, i32 0
  store i8* getelementptr inbounds ([4 x i8]* @str15, i32 0, i32 0), i8** %ptr21
  %tmp22 = getelementptr inbounds { i8**, i32 }* %x, i32 0, i32 0
  %tmpar23 = load i8*** %tmp22
  %ptr24 = getelementptr inbounds i8** %tmpar23, i32 0
  %item25 = load i8** %ptr24
  %printf26 = call i32 (i8*, ...)* @printf(i8* %item25)
  %x27 = load { i8**, i32 }* %x
  %z = alloca { i8**, i32 }
  store { i8**, i32 } %x27, { i8**, i32 }* %z
  %tmp28 = getelementptr inbounds { i8**, i32 }* %z, i32 0, i32 0
  %tmpar29 = load i8*** %tmp28
  %ptr30 = getelementptr inbounds i8** %tmpar29, i32 0
  store i8* getelementptr inbounds ([4 x i8]* @str16, i32 0, i32 0), i8** %ptr30
  %tmp31 = getelementptr inbounds { i8**, i32 }* %x, i32 0, i32 0
  %tmpar32 = load i8*** %tmp31
  %ptr33 = getelementptr inbounds i8** %tmpar32, i32 0
  %item34 = load i8** %ptr33
  %printf35 = call i32 (i8*, ...)* @printf(i8* %item34)
  ret i32 0
}
