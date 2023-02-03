module PracticingDataTypes where

    data StatusDescription = StatusDescription String deriving (Eq, Show)

    data StatusId = StatusId Int deriving (Eq, Show)

    data Price = Price Integer deriving (Eq, Show)

    data Status = Status StatusDescription StatusId deriving (Eq, Show)

    data TransactionStatus = Approved Status | Denied Status deriving (Eq, Show)

    approvedTransaction = Approved (Status (StatusDescription "Approved") (StatusId 1))
    deniedTransaction = Denied (Status (StatusDescription "Denied") (StatusId 2))

    data Transaction = Transaction Price TransactionStatus

    isTransactionApproved :: Transaction -> Bool
    isTransactionApproved (Transaction _ (Approved _)) = True
    isTransactionApproved (Transaction _ _) = False

    checkStatusId :: TransactionStatus -> Int
    checkStatusId (Approved (Status _ (StatusId statusId))) = statusId
    checkStatusId (Denied (Status _ (StatusId statusId))) = statusId

    checkStatusDescription :: TransactionStatus -> String
    checkStatusDescription (Approved (Status (StatusDescription description) _)) = description
    checkStatusDescription (Denied (Status (StatusDescription description) _)) = description

    processTransaction :: Transaction -> Bool
    processTransaction = isTransactionApproved