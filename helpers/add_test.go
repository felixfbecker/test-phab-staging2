package helpers

import "testing"

func TestAdd(t *testing.T) {
	sum := Add(1, 2, 3)
	if sum != 6 {
		t.Errorf("got %d, expected %d", sum, 6)
	}
}

func TestSubtract(t *testing.T) {
	diff := Subtract(1, 2, 3)
	if diff != -4 {
		t.Errorf("got %d, expected %d", diff, -4)
	}
}
