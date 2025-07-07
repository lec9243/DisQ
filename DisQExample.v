Require Import Reals.
Require Import Psatz.
Require Import Complex.
Require Import SQIR.
Require Import VectorStates UnitaryOps Coq.btauto.Btauto Coq.NArith.Nnat Permutation. 
Require Import Dirac.
Require Import QPE.
Require Import BasicUtility.
Require Import Classical_Prop.
Require Import MathSpec.
Require Import DisQSyntax.
Require Import DisQDef.
Require Import DisQKind.
Require Import DisQSem.
Require Import DisQType.
(**********************)
(** Unitary Programs **)
(**********************)

Require Import Coq.FSets.FMapList.
Require Import Coq.FSets.FMapFacts.
Require Import Coq.Structures.OrderedTypeEx.
Declare Scope pexp_scope.
Delimit Scope pexp_scope with pexp.
Local Open Scope pexp_scope.
Local Open Scope nat_scope.


Definition l:var := 1.
Definition u:var := 2.
Definition r:var := 3.
Definition n:nat := 8.
Definition b:nat := 7.
Definition q:nat := 2.
Definition x:var := 10.
Definition y:var := 11.
Definition c:var := 100.
Definition c1:var := 101.
Definition w:var := 1001.
Definition z:var := 1002.

Definition shor_seq := (Memb l ((AP (CAppU ((x, BNum 0, BNum n)::[]) (H x (BA x))) (AP (CAppU ((y, BNum 0, BNum n)::[]) (CU x (BA x) (RZ q y (BA y)))) (AP (CAppU ((x, BNum 0, BNum n)::[]) (QFT x b)) (PNil))))::[]))::[].

Definition shor_disq_process_l (i:nat) := AP (CAppU ((x, BNum i, BNum (i+1))::[]) (H x (BA x))) (DP (NewCh c 1) (DP (Send c (BA x)) PNil)).
Definition shor_disq_process_u  := DP (NewCh c 1) ((DP (Recv c w) (AP (CAppU ((y, BNum 0, BNum n)::[]) (CU w (BA w) (RZ q y (BA y)))) (DP (NewCh c1 1) (DP (Send c1 w) PNil))))). 
Definition shor_disq_process1_r  := (DP (NewCh c1 1) (DP (Recv c1 z) (PNil))).
