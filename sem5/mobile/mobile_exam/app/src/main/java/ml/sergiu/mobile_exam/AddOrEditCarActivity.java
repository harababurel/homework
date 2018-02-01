package ml.sergiu.mobile_exam;

import android.os.Bundle;
import android.support.v7.app.AppCompatActivity;
import android.util.Log;
import android.view.View;
import android.widget.EditText;
import android.widget.NumberPicker;

import org.json.JSONException;

import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

public class AddOrEditCarActivity extends AppCompatActivity {
    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_add_or_edit_car);

        NumberPicker quantity_picker = (NumberPicker) findViewById(R.id.quantity_picker);
        quantity_picker.setMinValue(0);
        quantity_picker.setMaxValue(100);
        quantity_picker.setWrapSelectorWheel(false);

        List<String> range = IntStream.rangeClosed(0, 100).boxed().map(x -> Integer.toString(x)).collect(Collectors.toList());
        quantity_picker.setDisplayedValues(range.toArray(new String[101]));

        Optional<Car> maybe_car = Optional.ofNullable((Car) getIntent().getSerializableExtra("car"));
        maybe_car.ifPresent(car -> {
            EditText id_edit_text = (EditText) findViewById(R.id.id_edit_text);
            id_edit_text.setText(Integer.toString(car.id));
            id_edit_text.setVisibility(View.VISIBLE);
            id_edit_text.setEnabled(false);

            EditText name_edit_text = (EditText) findViewById(R.id.name_edit_text);
            name_edit_text.setText(car.name);

            quantity_picker.setValue(car.quantity);

            EditText type_edit_text = (EditText) findViewById(R.id.type_edit_text);
            type_edit_text.setText(car.type);

            EditText status_edit_text = (EditText) findViewById(R.id.status_edit_text);
            status_edit_text.setText(car.status);
        });
    }

    public void confirm(View view) throws JSONException {
        Car car = new Car();

        EditText id_edit_text = (EditText) findViewById(R.id.id_edit_text);
        try {
            car.id = Integer.parseInt(id_edit_text.getText().toString());
        } catch(Exception e) {

        }

        EditText name_edit_text = (EditText) findViewById(R.id.name_edit_text);
        car.name = name_edit_text.getText().toString();

        NumberPicker quantity_picker = (NumberPicker) findViewById(R.id.quantity_picker);
        car.quantity = quantity_picker.getValue();

        EditText type_edit_text = (EditText) findViewById(R.id.type_edit_text);
        car.type = type_edit_text.getText().toString();

        EditText status_edit_text = (EditText) findViewById(R.id.status_edit_text);
        car.status = status_edit_text.getText().toString();

        Log.d("BOBS", "Constructed car: " + car);

        if (getIntent().getStringExtra("operation").equals("add")) {
            CarClient.addCar(Optional.empty(), car);
        } else {
            CarClient
            // edit car
        }


        onBackPressed();
    }

}
