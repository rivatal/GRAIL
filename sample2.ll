; ModuleID = 'Grail'

@fmt = private unnamed_addr constant [4 x i8] c"%d\0A\00"
@fmt1 = private unnamed_addr constant [4 x i8] c"%f\0A\00"

declare i32 @printf(i8*, ...)

declare i32 @sample_display(i32)

define void @f() {
entry:
  %0 = alloca { i32 }
  %ptr = getelementptr inbounds { i32 }* %0, i32 0, i32 0
  store i32 4, i32* %ptr
  %1 = load { i32 }* %0
  %a = alloca { i32 }
  store { i32 } %1, { i32 }* %a
  %a1 = load { i32 }* %a
  %strct = alloca { i32 }
  %strct2 = alloca { i32 }
  store { i32 } %a1, { i32 }* %strct2
  %tmp = getelementptr inbounds { i32 }* %strct2, i32 0, i32 0
  %val = load i32* %tmp
  %tmp3 = getelementptr inbounds { i32 }* %strct, i32 0, i32 0
  store i32 %val, i32* %tmp3
  %rec = load { i32 }* %strct
  %b = alloca { i32 }
  store { i32 } %rec, { i32 }* %b
  %ptr4 = getelementptr inbounds { i32 }* %b, i32 0, i32 0
  store i32 5, i32* %ptr4
  %2 = alloca { i32 }
  %ptr5 = getelementptr inbounds { i32 }* %2, i32 0, i32 0
  store i32 2, i32* %ptr5
  %3 = load { i32 }* %2
  %c = alloca { i32 }
  store { i32 } %3, { i32 }* %c
  %4 = alloca { i32 }
  %ptr6 = getelementptr inbounds { i32 }* %4, i32 0, i32 0
  store i32 2, i32* %ptr6
  %5 = load { i32 }* %4
  %d = alloca { i32 }
  store { i32 } %5, { i32 }* %d
  %y = alloca i32
  store i32 5, i32* %y
  %c7 = load { i32 }* %c
  %d8 = load { i32 }* %d
  %strct9 = alloca { i32 }
  store { i32 } %c7, { i32 }* %strct9
  %strct10 = alloca { i32 }
  store { i32 } %d8, { i32 }* %strct10
  %tmp11 = getelementptr inbounds { i32 }* %strct10, i32 0, i32 0
  %val12 = load i32* %tmp11
  %tmp13 = getelementptr inbounds { i32 }* %strct9, i32 0, i32 0
  %val14 = load i32* %tmp13
  %tmp15 = icmp eq i32 %val14, %val12
  %tmp16 = mul i1 %tmp15, true
  br i1 %tmp16, label %then, label %else

merge:                                            ; preds = %else, %then
  %6 = alloca { i32 }
  %ptr17 = getelementptr inbounds { i32 }* %6, i32 0, i32 0
  store i32 1, i32* %ptr17
  %7 = load { i32 }* %6
  %8 = alloca { { i32 }*, { i32 }*, i1, { i32 } }
  %ptr18 = getelementptr inbounds { { i32 }*, { i32 }*, i1, { i32 } }* %8, i32 0, i32 0
  store { i32 }* %a, { i32 }** %ptr18
  %ptr19 = getelementptr inbounds { { i32 }*, { i32 }*, i1, { i32 } }* %8, i32 0, i32 1
  store { i32 }* %c, { i32 }** %ptr19
  %ptr20 = getelementptr inbounds { { i32 }*, { i32 }*, i1, { i32 } }* %8, i32 0, i32 2
  store i1 false, i1* %ptr20
  %ptr21 = getelementptr inbounds { { i32 }*, { i32 }*, i1, { i32 } }* %8, i32 0, i32 3
  store { i32 } %7, { i32 }* %ptr21
  %9 = load { { i32 }*, { i32 }*, i1, { i32 } }* %8
  %e = alloca { { i32 }*, { i32 }*, i1, { i32 } }
  store { { i32 }*, { i32 }*, i1, { i32 } } %9, { { i32 }*, { i32 }*, i1, { i32 } }* %e
  ret void

then:                                             ; preds = %entry
  store i32 3, i32* %y
  br label %merge

else:                                             ; preds = %entry
  br label %merge
}
