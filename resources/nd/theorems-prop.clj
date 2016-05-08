; derived rules for the propositional logic
{:name "mt",
 :given [(impl a b) (not b)],
 :conclusion [(not a)],
 :forwards true,
 :proof [{:rule :premise, :id 1, :body (impl a b)}
         {:rule :premise, :id 2, :body (not b)}
         [{:id 5, :body a, :rule :assumption}
          {:id 8, :body b, :rule "\"impl-e\" (1 5)"}
          {:id 9, :body contradiction, :rule "\"not-e\" (2 8)"}]
         {:id 4, :body (not a), :rule "\"not-i\" ([5 9])"}]}

{:name "tnd",
 :given [],
 :conclusion [(or P (not P))],
 :forwards true,
 :proof [[{:id 3, :body (not (or P (not P))), :rule :assumption}
          [{:id 9, :body P, :rule :assumption}
           {:id 12, :body (or P (not P)), :rule "\"or-i1\" (9) []"}
           {:id 13, :body contradiction, :rule "\"not-e\" (3 12) []"}]
          {:id 8, :body (not P), :rule "\"not-i\" ([9 13]) []"}
          {:id 7, :body (or P (not P)), :rule "\"or-i2\" (8) []"}
          {:id 5, :body contradiction, :rule "\"not-e\" (7 3) []"}]
         {:id 2, :body (or P (not P)), :rule "\"raa\" ([3 5]) []"}]}

{:name "notnot-i",
 :given [P],
 :conclusion [(not (not P))],
 :forwards true,
 :proof [{:id 1, :body P, :rule :premise}
         [{:id 4, :body (not P), :rule :assumption}
          {:id 6, :body contradiction, :rule "\"not-e\" (1 4) []"}]
         {:id 3, :body (not (not P)), :rule "\"not-i\" ([4 6]) []"}]}

{:name "notnot-e",
 :given [(not (not P))],
 :conclusion [P],
 :forwards true,
 :proof [{:id 1, :body (not (not P)), :rule :premise}
         [{:id 4, :body (not P), :rule :assumption}
          {:id 6, :body contradiction, :rule "\"not-e\" (1 4) []"}]
         {:id 3, :body P, :rule "\"raa\" ([4 6]) []"}]}

; Contraposition
{:name "contrapos",
 :given [(impl P Q)],
 :conclusion [(impl (not Q) (not P))],
 :forwards true,
 :proof [{:id 1, :body (impl P Q), :rule :premise}
         [{:id 4, :body (not Q), :rule :assumption}
          [{:id 7, :body P, :rule :assumption}
           {:id 10, :body Q, :rule "\"impl-e\" (1 7) []"}
           {:id 9, :body contradiction, :rule "\"not-e\" (4 10) []"}]
          {:id 6, :body (not P), :rule "\"not-i\" ([7 9]) []"}]
         {:id 3, :body (impl (not Q) (not P)), :rule "\"impl-i\" ([4 6]) []"}]}

; De Morgan
{:name "dm-not-and",
 :given [(not (and P Q))],
 :conclusion [(or (not P) (not Q))],
 :forwards true,
 :proof [{:id 1, :body (not (and P Q)), :rule :premise}
         [{:id 4, :body (not (or (not P) (not Q))), :rule :assumption}
          [{:id 11, :body (not Q), :rule :assumption}
           {:id 15, :body (or (not P) (not Q)), :rule "\"or-i2\" (11) []"}
           {:id 13, :body contradiction, :rule "\"not-e\" (15 4) []"}]
          [{:id 17, :body (not P), :rule :assumption}
           {:id 21, :body (or (not P) (not Q)), :rule "\"or-i1\" (17) []"}
           {:id 19, :body contradiction, :rule "\"not-e\" (21 4) []"}]
          {:id 9, :body P, :rule "\"raa\" ([17 19]) []"}
          {:id 10, :body Q, :rule "\"raa\" ([11 13]) []"}
          {:id 8, :body (and P Q), :rule "\"and-i\" (9 10) []"}
          {:id 6, :body contradiction, :rule "\"not-e\" (8 1) []"}]
         {:id 3, :body (or (not P) (not Q)), :rule "\"raa\" ([4 6]) []"}]}

{:name "dm-or-not",
 :given [(or (not P) (not Q))],
 :conclusion [(not (and P Q))],
 :forwards true,
 :proof [{:id 1, :body (or (not P) (not Q)), :rule :premise}
         [{:id 8, :body (not P), :rule :assumption}
          [{:id 11, :body (and P Q), :rule :assumption}
           {:id 14, :body P, :rule "\"and-e1\" (11) []"}
           {:id 15, :body contradiction, :rule "\"not-e\" (8 14) []"}]
          {:id 10, :body (not (and P Q)), :rule "\"not-i\" ([11 15]) []"}]
         [{:id 5, :body (not Q), :rule :assumption}
          [{:id 16, :body (and P Q), :rule :assumption}
           {:id 19, :body Q, :rule "\"and-e2\" (16) []"}
           {:id 20, :body contradiction, :rule "\"not-e\" (5 19) []"}]
          {:id 7, :body (not (and P Q)), :rule "\"not-i\" ([16 20]) []"}]
         {:id 3, :body (not (and P Q)), :rule "\"or-e\" ([5 7] [8 10] 1) []"}]}

{:name "dm-not-or",
 :given [(not (or P Q))],
 :conclusion [(and (not P) (not Q))],
 :forwards true,
 :proof [{:id 1, :body (not (or P Q)), :rule :premise}
         [{:id 6, :body P, :rule :assumption}
          {:id 9, :body (or P Q), :rule "\"or-i1\" (6) []"}
          {:id 10, :body contradiction, :rule "\"not-e\" (1 9) []"}]
         [{:id 11, :body Q, :rule :assumption}
          {:id 14, :body (or P Q), :rule "\"or-i2\" (11) []"}
          {:id 15, :body contradiction, :rule "\"not-e\" (1 14) []"}]
         {:id 4, :body (not P), :rule "\"not-i\" ([6 10]) []"}
         {:id 5, :body (not Q), :rule "\"not-i\" ([11 15]) []"}
         {:id 3, :body (and (not P) (not Q)), :rule "\"and-i\" (4 5) []"}]}

{:name "dm-and-not",
 :given [(and (not P) (not Q))],
 :conclusion [(not (or P Q))],
 :forwards true,
 :proof [{:id 1, :body (and (not P) (not Q)), :rule :premise}
         {:id 8, :body (not Q), :rule "\"and-e2\" (1) []"}
         {:id 7, :body (not P), :rule "\"and-e1\" (1) []"}
         [{:id 4, :body (or P Q), :rule :assumption}
          [{:id 13, :body Q, :rule :assumption}
           {:id 16, :body contradiction, :rule "\"not-e\" (8 13) []"}]
          [{:id 10, :body P, :rule :assumption}
           {:id 17, :body contradiction, :rule "\"not-e\" (7 10) []"}]
          {:id 6, :body contradiction, :rule "\"or-e\" ([10 17] [13 16] 4) []"}]
         {:id 3, :body (not (or P Q)), :rule "\"not-i\" ([4 6]) []"}]}

; more theorems
