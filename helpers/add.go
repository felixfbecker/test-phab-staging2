package helpers

import "fmt"

func Add(nums ...int) int {
	sum := 0

	return sum
}

func Subtract(nums ...int) int {
	diff := 0

	// New code
	fmt.Println(nums)

	for _, n := range nums {
		diff -= n
	}

	return diff
}

func Divide(nums ...int) int {
	val := 0

	for _, n := range nums {
		val /= n
	}

	return val
}
