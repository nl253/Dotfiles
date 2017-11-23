
class Main {

	public static void main(String... args) {
		int[] array = {1, 33, 2, 1, 33, 1, 23, 123, 22, 1, 90};
		sort(array);
		for (int i = 1; i < array.length; i++)
			assert array[i - 1] < array[i];
	}

	static void sort(int[] array) {
		sort(array, 0, array.length - 1);
	}

	static private void sort(int[] array, int left, int right) {
		int pivot = partition(array, left, right);
		sort(array, left, pivot - 1);
		sort(array, pivot, right);
	}

	static int partition(int[] array, int left, int right) {
		int pivot = array.length / 2;

		// the bounds are inclusive
		while (left < right) {

			// increment the pointer if already sorted, stop if not 
			while (array[left] <= array[pivot]) left++;
			while (array[right] > array[pivot]) right--;

			// swap 
			if (left < right) {
				int tmp = array[left];
				array[left] = array[right];
				array[right] = tmp;
			}
		}
		return pivot;
	}
}
// vim:ft=java:sw=2:ts=2:foldmethod=marker:foldmarker={,}:
