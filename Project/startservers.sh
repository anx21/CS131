for i in "Alford" "Bolden" "Parker" "Powell" "Hamilton"
do
  echo Starting $i
  python proxyherd.py $i &
done

