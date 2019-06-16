Fixpoint par (x : nat) : Prop :=
match x with
| 0 => True
| S n => match n with
| 0 => False
| S m => par m
end
end.

(* Probando par
Compute par 10. *)

(*Ejercicio 1*)
Fixpoint suma (x y : nat) : nat :=
match x with
| 0 => y
| S n => S (suma n y)
end.

(* Probando suma 
Compute suma 15 5.*)

(*EJEMPLO PRODUCTO DE DOS NATURALES.*)
Fixpoint prod (x y : nat) : nat :=
match x with
| 0 => 0
| S n => suma y (prod n y)
end.
 
(* Probando prod
Compute prod 2 5.*)

(*Ejercicio 2*)
Fixpoint odd (x:nat) : Prop :=
match x with
| 0 => False
| S n => match n with
| 0 => True
| S m => odd m
end
end.

(* Probando odd
Compute odd 101.*)

(*Ejercicio 3*)
Theorem proove_false_x_x : ~False.
Proof.
unfold not.
intros proove_false.
exact proove_false.
Qed.


Require Import Bool.

Definition eqb (b1 b2 : bool) : bool :=
match b1, b2 with
|true, true => true
|true, false => false
|false, true => false
|false, false => true
end.

(*Ejercicio 4*)
Definition ifb (b1 b2 b3 : bool) : bool :=
match b1  with
|true => b2
|false => b3
end.

Lemma neutro_aditivo: forall n:nat, 0+n = n.
Proof.
intros.
simpl.
reflexivity.
Qed.

(*Ejercicio 5*)
Lemma neutro_aditivo_derecha : forall n:nat, n+0 = n.
Proof.
intros.
induction n.
simpl.
reflexivity.
simpl.
rewrite IHn.
reflexivity.
Qed.

(*Ejercicio 6*)
Lemma neutro_mult : forall n:nat, (S 0)*n = n.
Proof.
intros.
simpl.
rewrite -> neutro_aditivo_derecha.
reflexivity.
Qed.

(*Ejercicio 7*)
Lemma neutro_mult_der : forall n:nat, n*(S 0) = n.
Proof.
intros.
induction n.
simpl.
reflexivity.
simpl.
rewrite IHn.
reflexivity.
Qed.

(*Ejercicio 8*)
Lemma suma_uno : forall n:nat, n + 1 = S n.
Proof.
intros.
induction n.
simpl.
reflexivity.
simpl.
rewrite IHn.
reflexivity.
Qed.

Lemma mult_cero : forall n : nat, 0 * n = 0.
Proof.
intros.
simpl.
reflexivity.
Qed.

(*Ejercicio 9*)
Lemma mult_cero_derecha : forall n : nat, n * 0 = 0.
Proof.
intros.
induction n.
simpl.
reflexivity.
simpl.
rewrite IHn.
reflexivity.
Qed.

(*Ejercicio 10*)
Lemma sucesor_suma : forall n m : nat, n + S m = S (n + m).
Proof.
intros.
induction n.
simpl.
reflexivity.
simpl.
rewrite IHn.
reflexivity.
Qed.

(*Ejercicio 11*)
Theorem conm_suma: forall n m: nat, n+m = m+n.
Proof.
intros.
induction n.
simpl.
rewrite -> neutro_aditivo_derecha.
reflexivity.
simpl.
rewrite IHn.
rewrite -> sucesor_suma.
reflexivity.
Qed.