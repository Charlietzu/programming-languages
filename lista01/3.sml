fun sumLists (headList1::tailList1, headList2::tailList2) = 
    headList1 + headList2::(sumLists(tailList1, tailList2))
        | sumLists (_, _) = [];

sumLists([2, 5, 10], [1, 15, 4]);