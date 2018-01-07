package helpers

func Add(nums ...int) int {
	sum := 0

	for _, n := range nums {
		sum += n
	}

	return sum
}

func Subtract(nums ...int) int {
	diff := 0

	for _, n := range nums {
		diff -= n
	}

	return diff
}
