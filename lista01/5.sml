fun cumSum(headList::secondElementList::tailList) = 
    headList::cumSum(headList + secondElementList::tailList)
        | cumSum(value::[]) = [value]
        | cumSum([]) = [];

cumSum([]); 