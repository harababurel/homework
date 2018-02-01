package ml.sergiu.mobile_exam;

import android.app.Activity;
import android.content.Intent;
import android.os.Bundle;
import android.support.v7.app.AppCompatActivity;
import android.support.v7.widget.LinearLayoutManager;
import android.support.v7.widget.RecyclerView;
import android.util.Log;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.TextView;

import java.net.URI;
import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.List;

public class MainActivity extends AppCompatActivity implements GetCarsListener, AddCarListener {
    CarAdapter adapter;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_main);

        RecyclerView recyclerView = findViewById(R.id.recycler_view);
        adapter = new CarAdapter(this);
        recyclerView.setLayoutManager(new LinearLayoutManager(this));
        recyclerView.setAdapter(adapter);

    }

    @Override
    protected void onStart() {
        super.onStart();
        CarClient.getCars(this);
    }

    @Override
    protected void onResume() {
        super.onResume();
        CarClient.getCars(this);
    }

    public void startAddOrEditCarActivity(View view) {
        Intent intent = new Intent(this, AddOrEditCarActivity.class);
        intent.putExtra("operation", "add");
        startActivity(intent);
    }

    @Override
    public void processGet(List<Car> cars) {
        Log.d("BOBS", "Got " + cars.size() + " cars.");
        adapter.setCars(cars);

    }

    @Override
    public void processAdd(Car car) {
        Log.d("BOBS", "Adding new car to adapter");
        adapter.cars.add(car);
    }


    private static class CarViewHolder extends RecyclerView.ViewHolder {
        public CarViewHolder(View itemView) {
            super(itemView);
        }
    }

    private static class CarAdapter extends RecyclerView.Adapter<CarViewHolder> implements RemoveCarListener {
        private List<Car> cars;
        private Activity sourceActivity;

        CarAdapter(Activity sourceActivity ) {
            cars = new ArrayList<>();
            this.sourceActivity = sourceActivity;
        }

        public void setCars(List<Car> cars) {
            this.cars = cars;
            notifyDataSetChanged();
        }

        @Override
        public CarViewHolder onCreateViewHolder(ViewGroup parent, int viewType) {
            View view = LayoutInflater.from(parent.getContext()).inflate(R.layout.car_item, null);
            return new CarViewHolder(view);
        }

        @Override
        public void onBindViewHolder(CarViewHolder holder, int position) {
            TextView name_label = (TextView) holder.itemView.findViewById(R.id.name_label);
            name_label.setText(cars.get(position).name);
//            name_label.setBackgroundColor(TRANSPARENT);

            name_label.setOnClickListener(view -> {
                Intent intent = new Intent(sourceActivity, AddOrEditCarActivity.class);
                intent.putExtra("car", cars.get(position));
                intent.putExtra("operation", "edit");
                sourceActivity.startActivity(intent);
            });

            name_label.setOnLongClickListener(view -> {
                CarClient.removeCar(this, cars.get(position));
                return true;
            });
        }

        @Override
        public int getItemCount() {
            return cars.size();
        }

        @Override
        public void processRemove(Car car) {
            Log.d("BOBS", "Adding new car to adapter");

            if (cars.remove(car)) {
                notifyDataSetChanged();
            }
        }
    }
}
