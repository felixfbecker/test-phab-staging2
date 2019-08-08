package helpers

import "fmt"

// nums is an int here
func Add(nums ...int) int {
	sum := 0

	return sum
}

// nums is an integer
func Subtract(nums ...int) int {
	diff := 0

	// New code
	fmt.Println(nums)

	for _, n := range nums {
		diff -= n
	}

	return diff
}

// nums is an integer too
func Divide(nums ...int) int {
	val := 0

	for _, n := range nums {
		val /= n
	}

	return val
}
