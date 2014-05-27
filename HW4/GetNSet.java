import java.util.concurrent.atomic.AtomicIntegerArray;

class GetNSet implements State{
	private AtomicIntegerArray value;
	private byte maxval;
	
	GetNSet(byte[] v) {
		int[] int_v = new int[v.length];
		for (int i = 0; i < v.length; i++)
			int_v[i] = v[i];
		value = new AtomicIntegerArray(int_v); 
		maxval = 127; 
	}

	GetNSet(byte[] v, byte m) { 
		int[] int_v = new int[v.length];
		//System.arraycopy(v, 0, int_v, 0, v.length);
		for (int i = 0; i < v.length; i++)
			int_v[i] = v[i];
		value = new AtomicIntegerArray(int_v); 
		maxval = m; 
	}
	
	public int size() { return value.length(); }

	public byte[] current() { 
		byte[] byte_array = new byte[value.length()];
		//System.arraycopy(value, 0, byte_array, 0, value.length());
		for (int i = 0; i < value.length(); i++)
			byte_array[i] = (byte) value.get(i);
		return byte_array;
	}

	public boolean swap(int i, int j) {
		if (value.get(i) <= 0 || value.get(j) >= maxval)
			return false;

		value.set(i, value.get(i)-1);
		value.set(j, value.get(j)+1);
		return true;
	}
}
