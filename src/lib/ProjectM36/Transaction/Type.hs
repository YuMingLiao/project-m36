module ProjectM36.Transaction.Types where
import ProjectM36.Base

uid :: Transaction -> TransactionId
uid (Transaction uid _ _) = uid

info :: Transaction -> TransactionInfo
info (Transaction _ info _) = info
