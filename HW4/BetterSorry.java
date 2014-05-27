import java.util.concurrent.atomic.AtomicInteger;

class BetterSorry implements State{
	private AtomicInteger[] value;
	private byte maxval;
	
	BetterSorry(byte[] v) { 
		maxval = 127;

		value = new AtomicInteger[v.length];
		for (int i = 0; i < v.length; i++) {
			value[i] = new AtomicInteger(v[i]);
		}
	}

	BetterSorry(byte[] v, byte m) { 
		maxval = m; 

		value = new AtomicInteger[v.length];
		for (int i = 0; i < v.length; i++) {
			value[i] = new AtomicInteger(v[i]);
		}
	}

	public int size() { return value.length; }

	public byte[] current() { 
		byte[] new_v = new byte[value.length];
		for (int i = 0; i < value.length; i++) {
			new_v[i] = (byte) value[i].intValue();
		}
		return new_v;
	}

	public boolean swap(int i, int j) {
		if (value[i].get() <= 0 || value[j].get() >= maxval) {
			return false;
		}
		value[i].getAndDecrement();	
		value[j].getAndIncrement();
		return true;
	}
}
