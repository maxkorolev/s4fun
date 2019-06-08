package bank

case class BankConfig(
    deposit: BankDepositConfig,
    withdrawal: BankWithdrawalConfig
)

case class BankDepositConfig(
    maxDepositPerDay: BigDecimal,
    maxDepositPerTransaction: BigDecimal,
    maxDepositFrequency: Int
)

case class BankWithdrawalConfig(
    maxWithdrawalPerDay: BigDecimal,
    maxWithdrawalPerTransaction: BigDecimal,
    maxWithdrawalFrequency: Int
)
