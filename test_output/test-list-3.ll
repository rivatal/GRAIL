; ModuleID = 'Grail'

@fmt = private unnamed_addr constant [4 x i8] c"%d\0A\00"
@fmt.1 = private unnamed_addr constant [4 x i8] c"%f\0A\00"
@fmt.2 = private unnamed_addr constant [4 x i8] c"%d\0A\00"
@fmt.3 = private unnamed_addr constant [4 x i8] c"%f\0A\00"
@fmt.4 = private unnamed_addr constant [4 x i8] c"%d\0A\00"
@fmt.5 = private unnamed_addr constant [4 x i8] c"%f\0A\00"
@fmt.6 = private unnamed_addr constant [4 x i8] c"%d\0A\00"
@fmt.7 = private unnamed_addr constant [4 x i8] c"%f\0A\00"
@str = private unnamed_addr constant [4 x i8] c"cat\00"
@str.8 = private unnamed_addr constant [5 x i8] c"bird\00"
@str.9 = private unnamed_addr constant [4 x i8] c"dog\00"
@str.10 = private unnamed_addr constant [4 x i8] c"dog\00"

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

define i32 @main() {
entry:
  %strct = alloca { i8**, i32 }
  %lst = alloca i8*, i32 2
  %ptr = getelementptr inbounds i8*, i8** %lst, i32 0
  store i8* getelementptr inbounds ([4 x i8], [4 x i8]* @str, i32 0, i32 0), i8** %ptr
  %ptr1 = getelementptr inbounds i8*, i8** %lst, i32 1
  store i8* getelementptr inbounds ([5 x i8], [5 x i8]* @str.8, i32 0, i32 0), i8** %ptr1
  %p0 = getelementptr inbounds { i8**, i32 }, { i8**, i32 }* %strct, i32 0, i32 0
  %p1 = getelementptr inbounds { i8**, i32 }, { i8**, i32 }* %strct, i32 0, i32 1
  store i8** %lst, i8*** %p0
  store i32 2, i32* %p1
  %lst2 = load { i8**, i32 }, { i8**, i32 }* %strct
  %x = alloca { i8**, i32 }
  store { i8**, i32 } %lst2, { i8**, i32 }* %x
  %tmp = getelementptr inbounds { i8**, i32 }, { i8**, i32 }* %x, i32 0, i32 0
  %tmpar = load i8**, i8*** %tmp
  %ptr3 = getelementptr inbounds i8*, i8** %tmpar, i32 0
  %item = load i8*, i8** %ptr3
  %printf = call i32 (i8*, ...) @printf(i8* %item)
  %x4 = load { i8**, i32 }, { i8**, i32 }* %x
  %strct5 = alloca { i8**, i32 }
  %strct6 = alloca { i8**, i32 }
  store { i8**, i32 } %x4, { i8**, i32 }* %strct6
  %tmp7 = getelementptr inbounds { i8**, i32 }, { i8**, i32 }* %strct6, i32 0, i32 1
  %len = load i32, i32* %tmp7
  %tmp8 = getelementptr inbounds { i8**, i32 }, { i8**, i32 }* %strct6, i32 0, i32 0
  %lst9 = load i8**, i8*** %tmp8
  %tmp10 = getelementptr inbounds { i8**, i32 }, { i8**, i32 }* %strct5, i32 0, i32 1
  store i32 %len, i32* %tmp10
  %lst11 = alloca i8*, i32 %len
  %ind = alloca i32
  store i32 0, i32* %ind
  br label %checklimits

checklimits:                                      ; preds = %assignment, %entry
  %tmp17 = load i32, i32* %ind
  %comp = icmp slt i32 %tmp17, %len
  br i1 %comp, label %assignment, label %merge

assignment:                                       ; preds = %checklimits
  %i = load i32, i32* %ind
  %ptr12 = getelementptr inbounds i8*, i8** %lst9, i32 %i
  %ptr13 = getelementptr inbounds i8*, i8** %lst11, i32 %i
  %tmp14 = load i8*, i8** %ptr12
  %tmp15 = load i8*, i8** %ptr12
  store i8* %tmp15, i8** %ptr13
  %tmp16 = load i32, i32* %ind
  %inc = add i32 %tmp16, 1
  store i32 %inc, i32* %ind
  br label %checklimits

merge:                                            ; preds = %checklimits
  %tmp18 = getelementptr inbounds { i8**, i32 }, { i8**, i32 }* %strct5, i32 0, i32 0
  store i8** %lst11, i8*** %tmp18
  %strct19 = load { i8**, i32 }, { i8**, i32 }* %strct5
  %y = alloca { i8**, i32 }
  store { i8**, i32 } %strct19, { i8**, i32 }* %y
  %tmp20 = getelementptr inbounds { i8**, i32 }, { i8**, i32 }* %y, i32 0, i32 0
  %tmpar21 = load i8**, i8*** %tmp20
  %ptr22 = getelementptr inbounds i8*, i8** %tmpar21, i32 0
  store i8* getelementptr inbounds ([4 x i8], [4 x i8]* @str.9, i32 0, i32 0), i8** %ptr22
  %tmp23 = getelementptr inbounds { i8**, i32 }, { i8**, i32 }* %x, i32 0, i32 0
  %tmpar24 = load i8**, i8*** %tmp23
  %ptr25 = getelementptr inbounds i8*, i8** %tmpar24, i32 0
  %item26 = load i8*, i8** %ptr25
  %printf27 = call i32 (i8*, ...) @printf(i8* %item26)
  %x28 = load { i8**, i32 }, { i8**, i32 }* %x
  %z = alloca { i8**, i32 }
  store { i8**, i32 } %x28, { i8**, i32 }* %z
  %tmp29 = getelementptr inbounds { i8**, i32 }, { i8**, i32 }* %z, i32 0, i32 0
  %tmpar30 = load i8**, i8*** %tmp29
  %ptr31 = getelementptr inbounds i8*, i8** %tmpar30, i32 0
  store i8* getelementptr inbounds ([4 x i8], [4 x i8]* @str.10, i32 0, i32 0), i8** %ptr31
  %tmp32 = getelementptr inbounds { i8**, i32 }, { i8**, i32 }* %x, i32 0, i32 0
  %tmpar33 = load i8**, i8*** %tmp32
  %ptr34 = getelementptr inbounds i8*, i8** %tmpar33, i32 0
  %item35 = load i8*, i8** %ptr34
  %printf36 = call i32 (i8*, ...) @printf(i8* %item35)
  ret i32 0
}
