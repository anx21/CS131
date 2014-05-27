import java.util.concurrent.locks.ReentrantLock;

class BetterSafe implements State{
	private byte[] value;
	private byte maxval;
	private ReentrantLock my_lock;
	
	BetterSafe(byte[] v) { 
		value = v; maxval = 127;
		my_lock = new ReentrantLock();
	}

	BetterSafe(byte[] v, byte m) { 
		value = v; maxval = m; 
		my_lock = new ReentrantLock();
	}

	public int size() { return value.length; }

	public byte[] current() { 
		return value;
	}

	public boolean swap(int i, int j) {
		my_lock.lock();
		if (value[i] <= 0 || value[j] >= maxval) {
			my_lock.unlock();
			return false;
		}
		value[i]--;
		value[j]++;
		my_lock.unlock();
		return true;
	}
}
