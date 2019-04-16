-- Next Copyright AFAS Software BV - 2017


import Next
import NextKpi

personEntity = LAMEntity {lamEntname = "Person", lamEntType = Person}
customerStyle = LAMActorStyle {lamActorEnttype = Person, lamActorEntity = personEntity}
roleCustomer = LAMRole {lamRoleName = "Customer", lamActStyle = [customerStyle]}
roleSupplier = LAMRole {lamRoleName = "Supplier", lamActStyle = [customerStyle]}

goodEntity = LAMEntity {lamEntname = "Good", lamEntType = Good}
productStyle = LAMActorStyle {lamActorEnttype = Good, lamActorEntity = goodEntity}
roleProduct = LAMRole {lamRoleName = "Product", lamActStyle = [productStyle]}

-- unal : before we used paymentRequired as a separator between invoice and delivery. If we say paymentRequired=Always for a delivery then there will be no difference between it and an invoice
deliveryEvent = LAMEvent {lamEvtName = "Delivery", lamEvtStyle = Regular, lamEvtHasParty = True, lamEvtParty = roleCustomer, lamEvtHasSubject = True, lamEvtSubject = roleProduct, lamEvtHasAmount = True, lamEvtHasValue = True, lamEvtDirection = DirOut, lamEvtChangeOfOwnership = Always, lamEvtTypeOfOwnership = TradeItem, lamEvtPaymentRequired = Never}

goodsReceiveEvent = LAMEvent {lamEvtName = "GoodsReceive", lamEvtStyle = Regular, lamEvtHasParty = True, lamEvtParty = roleSupplier, lamEvtHasSubject = True, lamEvtSubject = roleProduct, lamEvtHasAmount = True, lamEvtHasValue = True, lamEvtDirection = DirIn, lamEvtChangeOfOwnership = Always, lamEvtTypeOfOwnership = TradeItem, lamEvtPaymentRequired = Never}

salesInvoiceEvent = LAMEvent {lamEvtName = "SalesInvoice", lamEvtStyle = Regular, lamEvtHasParty = True, lamEvtParty = roleCustomer, lamEvtHasSubject = True, lamEvtSubject = roleProduct, lamEvtHasAmount = True, lamEvtHasValue = True, lamEvtDirection = DirOut, lamEvtChangeOfOwnership = Never, lamEvtTypeOfOwnership = TradeItem, lamEvtPaymentRequired = Always}

salesOfferEvent = LAMEvent {lamEvtName = "salesOfferEvent", lamEvtStyle = Regular, lamEvtHasParty = True, lamEvtParty = roleCustomer, lamEvtHasSubject = True, lamEvtSubject = roleProduct, lamEvtHasAmount = True, lamEvtHasValue = True, lamEvtDirection = DirOut, lamEvtChangeOfOwnership = Never, lamEvtTypeOfOwnership = TradeItem, lamEvtPaymentRequired = Never}

salesOrderAgreement = LAMAgreement {lamAgrName = "salesOrderAgreement", lamAgrStyle = Agreement, lamAgrHasParty = True, lamAgrParty = roleCustomer, lamAgrHasAmount = True, lamAgrHasValue = True, lamAgrFormer = (Just salesOfferEvent), lamAgrLatter = deliveryEvent}


------------------------------------------- Run-time
person1 = EntityInstance{entRef = personEntity, entInstName = "Alice", entInstType = Person, entInstCreate = (1, 1, 1988), entInstDelete = emptyDateLast}
person2 = EntityInstance{entRef = personEntity, entInstName = "Bob", entInstType = Person, entInstCreate = (15, 2, 1988), entInstDelete = emptyDateLast}

customer1 = RoleInstance{rolRef = roleCustomer, rolInstName = "Customer", actInstStyle = [customerStyle], rolEnt = person1, rolInstCreate = (1, 1, 2016), rolInstDelete = emptyDateLast}
customer2 = RoleInstance{rolRef = roleCustomer, rolInstName = "Customer", actInstStyle = [customerStyle], rolEnt = person2, rolInstCreate = (1, 6, 2016), rolInstDelete = emptyDateLast}

supplier1 = RoleInstance{rolRef = roleSupplier, rolInstName = "Supplier", actInstStyle = [customerStyle], rolEnt = person1, rolInstCreate = (1, 1, 2016), rolInstDelete = emptyDateLast}

good1 = EntityInstance{entRef = goodEntity, entInstName = "Fiets", entInstType = Good, entInstCreate = (1, 1, 2016), entInstDelete = emptyDateLast}
good2 = EntityInstance{entRef = goodEntity, entInstName = "Auto", entInstType = Good, entInstCreate = (15, 2, 2016), entInstDelete = emptyDateLast}

product1 = RoleInstance{rolRef = roleProduct, rolInstName = "Product", actInstStyle = [productStyle], rolEnt = good1, rolInstCreate = (1, 1, 2016), rolInstDelete = emptyDateLast}
product2 = RoleInstance{rolRef = roleProduct, rolInstName = "Product", actInstStyle = [productStyle], rolEnt = good2, rolInstCreate = (15, 2, 2016), rolInstDelete = emptyDateLast}

delivery1a = EventInstance{evtRef = deliveryEvent, evtInstName = "delivery1a", evtInstStyle = Regular, evtInstHasParty = True, evtInstParty = customer1, evtInstHasSubject = True, evtInstSubject = product1, evtInstHasAmount = True, evtInstHasValue = True, evtInstDirection  = DirOut, evtInstChangeOfOwnership = Always, evtInstTypeOfOwnership = TradeItem, evtInstPaymentRequired = Never, evtInstPlanned = Timing { todo = (1, 1, 2017), begin = (5, 1, 2017), end = (10, 2, 2017)}, evtInstActual = Timing{ todo = (3, 1, 2017), begin = (10, 2, 2017), end = (16, 3, 2017)}, evtInstAdministrative = Timing { todo = (1, 1, 2017), begin = (5, 1, 2017),  end = (10, 2, 2017)}, evtInstAmount = 5, evtInstValue = [100, 500]}

delivery1b = EventInstance{evtRef = deliveryEvent, evtInstName = "delivery1b", evtInstStyle = Regular, evtInstHasParty = True, evtInstParty = customer1, evtInstHasSubject = True, evtInstSubject = product1, evtInstHasAmount = True, evtInstHasValue = True, evtInstDirection  = DirOut, evtInstChangeOfOwnership = Always, evtInstTypeOfOwnership = TradeItem, evtInstPaymentRequired = Never, evtInstPlanned = Timing{ todo = (11, 3, 2017), begin = (15, 3, 2017), end = (20, 5, 2017)}, evtInstActual = Timing{ todo = (13, 3, 2017), begin = (20, 5, 2017), end = (26, 6, 2017)}, evtInstAdministrative = Timing{ todo = (11, 3, 2017), begin = (15, 3, 2017), end = (20, 5, 2017)}, evtInstAmount = 3, evtInstValue = [100, 500]}

delivery2a = EventInstance{evtRef = deliveryEvent, evtInstName = "delivery2a", evtInstStyle = Regular, evtInstHasParty = True, evtInstParty = customer2, evtInstHasSubject = True, evtInstSubject = product2, evtInstHasAmount = True, evtInstHasValue = True, evtInstDirection  = DirOut, evtInstChangeOfOwnership = Always, evtInstTypeOfOwnership = TradeItem, evtInstPaymentRequired = Never, evtInstPlanned = Timing{ todo =(1, 2, 2016), begin = (5, 2, 2016), end =(10, 3, 2016)}, evtInstActual = Timing{ todo = (3, 2, 2016), begin = (10, 3, 2016),  end = (30, 3, 2016)}, evtInstAdministrative = Timing{ todo = (1, 2, 2016), begin = (5, 2, 2016), end = (10, 3, 2016)}, evtInstAmount = 5, evtInstValue = [1000, 5000]}

delivery2b = EventInstance{evtRef = deliveryEvent, evtInstName = "delivery2b", evtInstStyle = Regular, evtInstHasParty = True, evtInstParty = customer2, evtInstHasSubject = True, evtInstSubject = product2, evtInstHasAmount = True, evtInstHasValue = True, evtInstDirection  = DirOut, evtInstChangeOfOwnership = Always, evtInstTypeOfOwnership = TradeItem, evtInstPaymentRequired = Never, evtInstPlanned = Timing{ todo = (11, 5, 2016), begin = (15, 5, 2016), end = (20, 6, 2016)}, evtInstActual = Timing{ todo = (13, 5, 2016), begin = (20, 6, 2016), end = (30, 7, 2016)}, evtInstAdministrative = Timing{ todo = (11, 5, 2016), begin = (15, 5, 2016), end = (20, 6, 2016)}, evtInstAmount = 7, evtInstValue = [1000, 5000]}

delivery3a = EventInstance{evtRef = deliveryEvent, evtInstName = "delivery3a", evtInstStyle = Regular, evtInstHasParty = True, evtInstParty = customer2, evtInstHasSubject = True, evtInstSubject = product2, evtInstHasAmount = True, evtInstHasValue = True, evtInstDirection  = DirOut, evtInstChangeOfOwnership = Always, evtInstTypeOfOwnership = TradeItem, evtInstPaymentRequired = Never, evtInstPlanned = Timing{ todo = (1, 5, 2016), begin = (5, 5, 2016), end = (10, 6, 2016)}, evtInstActual = emptyTiming, evtInstAdministrative = Timing{ todo = (1, 5, 2016), begin = (5, 5, 2016), end = (10, 6, 2016)}, evtInstAmount = 7, evtInstValue = [1000, 5000]}

delivery3b = EventInstance{evtRef = deliveryEvent, evtInstName = "delivery3b", evtInstStyle = Regular, evtInstHasParty = True, evtInstParty = customer2, evtInstHasSubject = True, evtInstSubject = product2, evtInstHasAmount = True, evtInstHasValue = True, evtInstDirection  = DirOut, evtInstChangeOfOwnership = Always, evtInstTypeOfOwnership = TradeItem, evtInstPaymentRequired = Never, evtInstPlanned = emptyTiming, evtInstActual = emptyTiming, evtInstAdministrative = Timing{ todo = (1, 5, 2016), begin = (5, 5, 2016), end = (10, 6, 2016)}, evtInstAmount = 7, evtInstValue = [1000, 5000]}

goodReceive1a = EventInstance{evtRef = goodsReceiveEvent, evtInstName = "goodReceive1a", evtInstStyle = Regular, evtInstHasParty = True, evtInstParty = customer1, evtInstHasSubject = True, evtInstSubject = product1, evtInstHasAmount = True, evtInstHasValue = True, evtInstDirection  = DirIn, evtInstChangeOfOwnership = Always, evtInstTypeOfOwnership = TradeItem, evtInstPaymentRequired = Never, evtInstPlanned = Timing{ todo = (1, 8, 2017), begin = (5, 8, 2017), end = (10, 9, 2017)}, evtInstActual = Timing{ todo = (5, 8, 2017), begin = (10, 9, 2017), end = (16, 10, 2017)}, evtInstAdministrative = Timing{ todo = (1, 8, 2017), begin = (5, 8, 2017), end = (10, 9, 2017)}, evtInstAmount = 100, evtInstValue = [100, 500]}

goodReceive1b = EventInstance{evtRef = goodsReceiveEvent, evtInstName = "goodReceive1b", evtInstStyle = Regular, evtInstHasParty = True, evtInstParty = customer1, evtInstHasSubject = True, evtInstSubject = product1, evtInstHasAmount = True, evtInstHasValue = True, evtInstDirection  = DirIn, evtInstChangeOfOwnership = Always, evtInstTypeOfOwnership = TradeItem, evtInstPaymentRequired = Never, evtInstPlanned = Timing{ todo = (11, 8, 2017), begin = (15, 8, 2017), end = (20, 9, 2017)}, evtInstActual = Timing{ todo = (15, 8, 2017), begin = (10, 9, 2017), end = (26, 10, 2017)}, evtInstAdministrative = Timing{ todo = (11, 8, 2017), begin = (15, 8, 2017), end = (20, 9, 2017)}, evtInstAmount = 100, evtInstValue = [100, 500]}

goodReceive2a = EventInstance{evtRef = goodsReceiveEvent, evtInstName = "goodReceive2a", evtInstStyle = Regular, evtInstHasParty = True, evtInstParty = customer1, evtInstHasSubject = True, evtInstSubject = product1, evtInstHasAmount = True, evtInstHasValue = True, evtInstDirection  = DirIn, evtInstChangeOfOwnership = Always, evtInstTypeOfOwnership = TradeItem, evtInstPaymentRequired = Never, evtInstPlanned = Timing{ todo = (10, 8, 2016), begin = (20, 9, 2016), end = (30, 10, 2016)}, evtInstActual = Timing{ todo = (10, 8, 2016), begin = (20, 9, 2016), end = (30, 10, 2016)}, evtInstAdministrative = Timing{ todo = (10, 8, 2016), begin = (20, 9, 2016), end = (30, 10, 2016)}, evtInstAmount = 100, evtInstValue = [100, 500]}

goodReceive2b = EventInstance{evtRef = goodsReceiveEvent, evtInstName = "goodReceive2b", evtInstStyle = Regular, evtInstHasParty = True, evtInstParty = customer1, evtInstHasSubject = True, evtInstSubject = product1, evtInstHasAmount = True, evtInstHasValue = True, evtInstDirection  = DirIn, evtInstChangeOfOwnership = Always, evtInstTypeOfOwnership = TradeItem, evtInstPaymentRequired = Never, evtInstPlanned = Timing{ todo = (10, 8, 2016), begin = (20, 9, 2016), end = (30, 10, 2016)}, evtInstActual = Timing{ todo = (10, 8, 2016), begin = (20, 9, 2016), end = (30, 10, 2016)}, evtInstAdministrative = Timing { todo = (10, 8, 2016), begin = (20, 9, 2016), end = (30, 10, 2016)}, evtInstAmount = 100, evtInstValue = [100, 500]}

salesInvoice1a = EventInstance{evtRef = salesInvoiceEvent, evtInstName = "salesInvoice1a", evtInstStyle = Regular, evtInstHasParty = True, evtInstParty = customer1, evtInstHasSubject = True, evtInstSubject = product1, evtInstHasAmount = True, evtInstHasValue = True, evtInstDirection  = DirOut, evtInstChangeOfOwnership = Never, evtInstTypeOfOwnership = TradeItem, evtInstPaymentRequired = Always, evtInstPlanned = Timing{ todo = (1, 8, 2016), begin = (5, 8, 2016), end =(10, 9, 2016)}, evtInstActual = Timing{ todo = (5, 8, 2016), begin = (10, 9, 2016), end = (16, 10, 2016)}, evtInstAdministrative = Timing{ todo = (1, 8, 2016), begin = (5, 8, 2016), end = (10, 9, 2016)}, evtInstAmount = 100, evtInstValue = [100, 500]}

salesOffer1a = EventInstance{evtRef = salesOfferEvent, evtInstName = "salesOffer1a", evtInstStyle = Regular, evtInstHasParty = True, evtInstParty = customer1, evtInstHasSubject = True, evtInstSubject = product1, evtInstHasAmount = True, evtInstHasValue = True, evtInstDirection  = DirOut, evtInstChangeOfOwnership = Never, evtInstTypeOfOwnership = TradeItem, evtInstPaymentRequired = Never, evtInstPlanned = Timing{ todo = (1, 8, 2016), begin = (5, 8, 2016), end =(10, 9, 2016)}, evtInstActual = Timing{ todo = (5, 8, 2016), begin = (10, 9, 2016), end = (16, 10, 2016)}, evtInstAdministrative = Timing{ todo = (1, 8, 2016), begin = (5, 8, 2016), end = (10, 9, 2016)}, evtInstAmount = 100, evtInstValue = [100, 500]}

salesOffer1b = EventInstance{evtRef = salesOfferEvent, evtInstName = "salesOffer1b", evtInstStyle = Regular, evtInstHasParty = True, evtInstParty = customer1, evtInstHasSubject = True, evtInstSubject = product1, evtInstHasAmount = True, evtInstHasValue = True, evtInstDirection  = DirOut, evtInstChangeOfOwnership = Never, evtInstTypeOfOwnership = TradeItem, evtInstPaymentRequired = Never, evtInstPlanned = Timing{ todo = (1, 8, 2016), begin = (5, 8, 2016), end =(10, 9, 2016)}, evtInstActual = Timing{ todo = (5, 8, 2016), begin = (10, 9, 2016), end = (16, 10, 2016)}, evtInstAdministrative = Timing{ todo = (1, 8, 2016), begin = (5, 8, 2016), end = (10, 9, 2016)}, evtInstAmount = 100, evtInstValue = [100, 500]}

salesOrder1a = AgreementInstance{agrRef = salesOrderAgreement, agrInstName = "salesOrder1a", agrInstStyle = Agreement, agrInstHasParty = True, agrInstParty = customer1, agrInstHasAmount = True, agrInstHasValue = True,  agrInstPlanned = Timing{ todo = (1, 8, 2016), begin = (5, 8, 2016), end =(10, 9, 2016)}, agrInstActual = Timing{ todo = (5, 8, 2016), begin = (10, 9, 2016), end = (16, 10, 2016)}, agrInstAdministrative = Timing{ todo = (1, 8, 2016), begin = (5, 8, 2016), end = (10, 9, 2016)}, agrInstAmount = 100, agrInstValue = [100, 500], agrInstFormer = (Just salesOffer1a), agrInstLatter =  delivery1a}

salesOrder1b = AgreementInstance{agrRef = salesOrderAgreement, agrInstName = "salesOrder1b", agrInstStyle = Agreement, agrInstHasParty = True, agrInstParty = customer1, agrInstHasAmount = True, agrInstHasValue = True,  agrInstPlanned = Timing{ todo = (1, 8, 2016), begin = (5, 8, 2016), end =(10, 9, 2016)}, agrInstActual = Timing{ todo = (5, 8, 2016), begin = (10, 9, 2016), end = (16, 10, 2016)}, agrInstAdministrative = Timing{ todo = (1, 8, 2016), begin = (5, 8, 2016), end = (10, 9, 2016)}, agrInstAmount = 100, agrInstValue = [100, 500], agrInstFormer = Nothing, agrInstLatter =  delivery1b}

-- Sample populations
persons = [person1, person2]
goods = [good1, good2]
roles = [customer1, customer2, product1, product2, supplier1]
events = [delivery1a, delivery1b, delivery2a, delivery2b, delivery3a, delivery3b, goodReceive1a, goodReceive1b, goodReceive2a, goodReceive2b, salesOffer1a, salesOffer1b]
agreements = [salesOrder1a, salesOrder1b]


-- Show some KPI values
p1Begin = (1, 1, 2017)
p1End = (31, 12, 2017)
p2Begin = (1, 1, 2016)
p2End = (31, 12, 2016)

kpiValue_AverageDuration = kpiAvgDurChgOwDone_compare_with_same_period_last_year events p1Begin p1End p2Begin p2End

kpiValue_FromEventToAgreementPercentage = (show $ (kpiFromEventToAgreementPercentage events agreements) ) ++ " %" 



