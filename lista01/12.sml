fun multiPairs(headList1::tailList1, headList2::tailList2) = headList1 * headList2::(multiPairs(tailList1, tailList2)) | multiPairs(_, _) = [];

multiPairs([2, 5, 10], [4, 10, 8]);