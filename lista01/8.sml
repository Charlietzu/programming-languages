fun allTrue ([]) = false
    | allTrue (headList::[]) =  headList
    | allTrue (headList::tailList) = headList andalso allTrue(tailList);

allTrue([true, true, false, true]);    

allTrue([true, true, true]);