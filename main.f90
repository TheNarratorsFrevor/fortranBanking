program banking_system
implicit none

integer, parameter :: MAX_CUSTOMERS = 100
integer :: account_count = 0
integer :: customer_index, option
real :: amount, balance
character(len=20) :: name, account_number
character(len=20), dimension(MAX_CUSTOMERS) :: customer_names
real, dimension(MAX_CUSTOMERS) :: customer_balances

! Initialize customers
call add_customer("John", 1000.0)
call add_customer("Jane", 500.0)

! Main program loop
do
    write(*,*) "Welcome to the Banking System."
    write(*,*) "Please enter your account name (or 'exit' to quit): "
    read(*,*) account_number
    if (account_number == "exit") exit
    customer_index = find_customer(account_number)
    if (customer_index == 0) then
        write(*,*) "Account not found."
    else
        name = customer_names(customer_index)
        balance = customer_balances(customer_index)
        write(*,*) "Welcome, ", name, "!"
        do
            write(*,*) "Please select an option:"
            write(*,*) "1. Deposit"
            write(*,*) "2. Withdraw"
            write(*,*) "3. Check Balance"
            write(*,*) "4. Exit"
            read(*,*) option
            select case (option)
                case (1)
                    write(*,*) "Please enter deposit amount: "
                    read(*,*) amount
                    balance = balance + amount
                    write(*,*) "Deposited ", amount, " into account. New balance is ", balance
                case (2)
                    write(*,*) "Please enter withdrawal amount: "
                    read(*,*) amount
                    if (amount > balance) then
                        write(*,*) "Insufficient funds."
                    else
                        balance = balance - amount
                        write(*,*) "Withdrew ", amount, " from account. New balance is ", balance
                    end if
                case (3)
                    write(*,*) "Current account balance is ", balance
                case (4)
                    exit
                case default
                    write(*,*) "Invalid option. Please try again."
            end select
        end do
    end if
end do

contains

subroutine add_customer(name, balance)
character(len=*), intent(in) :: name
real, intent(in) :: balance
if (account_count >= MAX_CUSTOMERS) then
    write(*,*) "Maximum number of customers reached."
else
    account_count = account_count + 1
    customer_names(account_count) = name
    customer_balances(account_count) = balance
    write(*,*) "Added new customer: ", name
end if
end subroutine add_customer

function find_customer(account_number) result(customer_index)
character(len=*), intent(in) :: account_number
integer :: customer_index
customer_index = 0
do customer_index = 1, account_count
    if (customer_names(customer_index) == account_number) exit
end do
if (customer_index > account_count) customer_index = 0
end function find_customer

end program banking_system