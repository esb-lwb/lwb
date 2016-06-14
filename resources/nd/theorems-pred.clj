; De Morgan
{:name "not-forall->exists-not", :given [(not (forall [x] (P x)))], :conclusion [(exists [x] (not (P x)))], :forwards true, :proof [{:id 1, :body (not (forall [x] (P x))), :rule :premise} [{:id 4, :body (not (exists [x] (not (P x)))), :rule :assumption} [{:id 9, :body (actual :i), :rule :assumption} [{:id 12, :body (not (P :i)), :rule :assumption} {:id 16, :body (exists [x] (not (P x))), :rule "\"exists-i\" (12 9) []"} {:id 14, :body contradiction, :rule "\"not-e\" (16 4) []"}] {:id 11, :body (P :i), :rule "\"raa\" ([12 14]) []"}] {:id 8, :body (forall [x] (P x)), :rule "\"forall-i\" ([9 11]) []"} {:id 6, :body contradiction, :rule "\"not-e\" (8 1) []"}] {:id 3, :body (exists [x] (not (P x))), :rule "\"raa\" ([4 6]) []"}]}
{:name "exists-not->not-forall", :given [(exists [x] (not (P x)))], :conclusion [(not (forall [x] (P x)))], :forwards true, :proof [{:id 1, :body (exists [x] (not (P x))), :rule :premise} [{:id 4, :body (not (not (forall [x] (P x)))), :rule :assumption} {:id 7, :body (forall [x] (P x)), :rule "\"notnot-e\" (4) []"} [{:rule :assumption, :id 8, :body (actual :i)} {:rule :assumption, :id 9, :body (not (P :i))} {:id 14, :body (P :i), :rule "\"forall-e\" (7 8) []"} {:id 11, :body contradiction, :rule "\"not-e\" (14 9) []"}] {:id 6, :body contradiction, :rule "\"exists-e\" ([8 11] 1) []"}] {:id 3, :body (not (forall [x] (P x))), :rule "\"raa\" ([4 6]) []"}]}
{:name "not-exists->forall-not", :given [(not (exists [x] (P x)))], :conclusion [(forall [x] (not (P x)))], :forwards true, :proof [{:id 1, :body (not (exists [x] (P x))), :rule :premise} [{:id 4, :body (actual :i), :rule :assumption} [{:id 7, :body (P :i), :rule :assumption} {:id 11, :body (exists [x] (P x)), :rule "\"exists-i\" (7 4) []"} {:id 9, :body contradiction, :rule "\"not-e\" (11 1) []"}] {:id 6, :body (not (P :i)), :rule "\"not-i\" ([7 9]) []"}] {:id 3, :body (forall [x] (not (P x))), :rule "\"forall-i\" ([4 6]) []"}]}
{:name "forall-not->not-exists", :given [(forall [x] (not (P x)))], :conclusion [(not (exists [x] (P x)))], :forwards true, :proof [{:id 1, :body (forall [x] (not (P x))), :rule :premise} [{:id 4, :body (not (not (exists [x] (P x)))), :rule :assumption} {:id 7, :body (exists [x] (P x)), :rule "\"notnot-e\" (4) []"} [{:rule :assumption, :id 8, :body (actual :i)} {:rule :assumption, :id 9, :body (P :i)} {:id 12, :body (not (P :i)), :rule "\"forall-e\" (1 8) []"} {:id 13, :body contradiction, :rule "\"not-e\" (9 12) []"}] {:id 6, :body contradiction, :rule "\"exists-e\" ([8 13] 7) []"}] {:id 3, :body (not (exists [x] (P x))), :rule "\"raa\" ([4 6]) []"}]}
