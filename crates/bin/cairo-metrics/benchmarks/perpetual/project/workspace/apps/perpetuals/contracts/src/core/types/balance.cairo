use core::num::traits::Zero;
use core::traits::Neg;

#[derive(Copy, Debug, Default, Drop, PartialEq, Serde, starknet::Store)]
pub struct Balance {
    value: i64,
}

#[derive(Copy, Debug, Default, Drop, Serde)]
pub struct BalanceDiff {
    pub before: Balance,
    pub after: Balance,
}

impl BalancePartialOrd of PartialOrd<Balance> {
    fn lt(lhs: Balance, rhs: Balance) -> bool {
        lhs.value < rhs.value
    }
}

impl BalanceNeg of Neg<Balance> {
    fn neg(a: Balance) -> Balance {
        Balance { value: -(a.value) }
    }
}

impl BalanceAdd of Add<Balance> {
    fn add(lhs: Balance, rhs: Balance) -> Balance {
        Balance { value: lhs.value + rhs.value }
    }
}
impl BalanceSub of Sub<Balance> {
    fn sub(lhs: Balance, rhs: Balance) -> Balance {
        Balance { value: lhs.value - rhs.value }
    }
}

pub impl BalanceAddAssign of core::ops::AddAssign<Balance, Balance> {
    fn add_assign(ref self: Balance, rhs: Balance) {
        self.value += rhs.value;
    }
}

pub impl BalanceSubAssign of core::ops::SubAssign<Balance, Balance> {
    fn sub_assign(ref self: Balance, rhs: Balance) {
        self.value -= rhs.value;
    }
}

pub impl I64IntoBalance of Into<i64, Balance> {
    fn into(self: i64) -> Balance {
        Balance { value: self }
    }
}

pub impl U64IntoBalance of Into<u64, Balance> {
    fn into(self: u64) -> Balance {
        let value: i64 = self.try_into().expect('Balance value doesn\'t fit i64');
        Balance { value }
    }
}

pub impl BalanceIntoI164 of Into<Balance, i64> {
    fn into(self: Balance) -> i64 {
        self.value
    }
}

pub impl BalanceIntoI1128 of Into<Balance, i128> {
    fn into(self: Balance) -> i128 {
        self.value.into()
    }
}

pub impl BalanceZeroImpl of Zero<Balance> {
    fn zero() -> Balance {
        Balance { value: 0 }
    }

    fn is_zero(self: @Balance) -> bool {
        self.value.is_zero()
    }

    fn is_non_zero(self: @Balance) -> bool {
        self.value.is_non_zero()
    }
}

#[generate_trait]
pub impl BalanceImpl of BalanceTrait {
    fn new(value: i64) -> Balance {
        Balance { value }
    }
    fn add(self: Balance, other: i64) -> Balance {
        Balance { value: self.value + other }
    }
    fn sub(self: Balance, other: i64) -> Balance {
        Balance { value: self.value - other }
    }
}

#[cfg(test)]
mod tests {
    use core::num::traits::Zero;
    use super::{Balance, BalanceTrait};

    #[test]
    fn test_add() {
        let balance1 = Balance { value: 10 };
        let balance2 = Balance { value: 5 };
        let new_balance = balance1 + balance2;
        assert!(new_balance == BalanceTrait::new(value: 15));
    }

    #[test]
    fn test_sub() {
        let balance1 = Balance { value: 10 };
        let balance2 = Balance { value: 5 };
        let new_balance = balance1 - balance2;
        assert!(new_balance == BalanceTrait::new(value: 5));
    }


    #[test]
    fn test_add_assign() {
        let mut balance = Balance { value: 10 };
        balance += Balance { value: 5 };
        assert!(balance == BalanceTrait::new(value: 15));
    }

    #[test]
    fn test_sub_assign() {
        let mut balance = Balance { value: 10 };
        balance -= Balance { value: 5 };
        assert!(balance == BalanceTrait::new(value: 5));
    }

    #[test]
    fn test_i64_into_balance() {
        let balance: Balance = 10_i64.into();
        assert!(balance == BalanceTrait::new(value: 10));
    }

    #[test]
    fn test_balance_into_i64() {
        let balance: Balance = Balance { value: 10 };
        assert!(balance.into() == 10_i64);
    }

    #[test]
    fn test_zero() {
        let balance: Balance = Zero::zero();
        assert!(balance == BalanceTrait::new(value: 0));
    }

    #[test]
    fn test_is_zero() {
        let balance = Balance { value: 0 };
        assert!(balance.is_zero());
    }

    #[test]
    fn test_is_non_zero() {
        let balance = Balance { value: 10 };
        assert!(balance.is_non_zero());
    }

    #[test]
    fn test_add_u64() {
        let balance = Balance { value: 10 };
        let new_balance = balance.add(5);
        assert!(new_balance == BalanceTrait::new(value: 15));
    }
}
