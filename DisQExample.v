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

Definition shor_seq :=
  let proces :=
    AP (CAppU ((y, BNum 0, BNum n)::[]) (CU x (BA x) (RZ q y (BA y))))
       (AP (CAppU ((x, BNum 0, BNum n)::[]) (QFT x b)) PNil)
  in
  let fix nH (n : nat) :=
    match n with
    | 0 => proces
    | S k =>
        AP (CAppU ((x, BNum 0, BNum n)::[]) (H x (Num n))) (nH k)
    end
  in
  (Memb l [nH n])::[].
(*  (Memb l ((AP (CAppU ((x, BNum 0, BNum n)::[]) (H x (BA x))) (AP (CAppU ((y, BNum 0, BNum n)::[]) (CU x (BA x) (RZ q y (BA y)))) (AP (CAppU ((x, BNum 0, BNum n)::[]) (QFT x b)) (PNil))))::[]))::[].
 *)


Definition shor_disq_process_l :=
  let fix nH (i:nat) :=
    match i with
    | 0 => PNil
    | S k => AP (CAppU ((x, BNum 0, BNum n)::[]) (H x (Num k))) (DP (NewCh c 1) (DP (Send c (BA x)) (nH k)))
    end
  in nH n.

Definition shor_disq_process_u  :=
  let fix nCU (i:nat) :=
    match i with
    | 0 => PNil
    | S k => DP (NewCh c 1) ((DP (Recv c w) (AP (CAppU ((y, BNum 0, BNum n)::[]) (CU w (BA w) (RZ q y (BA y)))) (DP (NewCh c1 1) (DP (Send c1 w) PNil)))))
    end
  in nCU n.

Definition shor_disq_process_r :=
  let proces := (AP (CAppU ((x, BNum 0, BNum n)::[]) (QFT x b)) (PNil)) in 
  let fix recvN (i:nat) :=
    match i with
    | 0 => proces
    | S k => (DP (NewCh c1 1) (DP (Recv c1 z) (AP (CAppU ((x, BNum 0, BNum n)::[]) (Addto z x)) (recvN k))))
    end
  in recvN n.

Definition shor_dist := shor_disq_process_l :: shor_disq_process_u :: shor_disq_process_r :: [].
