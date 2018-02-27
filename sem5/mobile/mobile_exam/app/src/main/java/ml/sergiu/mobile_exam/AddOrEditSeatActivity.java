package ml.sergiu.mobile_exam;

import android.os.Bundle;
import android.support.v7.app.AppCompatActivity;
import android.util.Log;
import android.view.View;
import android.widget.EditText;
import android.widget.NumberPicker;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;
import java.util.stream.IntStream;
import org.json.JSONException;

public class AddOrEditSeatActivity extends AppCompatActivity {
  @Override
  protected void onCreate(Bundle savedInstanceState) {
    super.onCreate(savedInstanceState);
    setContentView(R.layout.activity_add_or_edit_seat);

    Optional<Seat> maybe_seat =
        Optional.ofNullable((Seat) getIntent().getSerializableExtra("seat"));
    maybe_seat.ifPresent(
        seat -> {
          EditText id_edit_text = (EditText) findViewById(R.id.id_edit_text);
          id_edit_text.setText(Integer.toString(seat.id));
          id_edit_text.setVisibility(View.VISIBLE);
          id_edit_text.setEnabled(false);

          EditText name_edit_text = (EditText) findViewById(R.id.name_edit_text);
          name_edit_text.setText(seat.name);

          EditText type_edit_text = (EditText) findViewById(R.id.type_edit_text);
          type_edit_text.setText(seat.type);

          EditText status_edit_text = (EditText) findViewById(R.id.status_edit_text);
          status_edit_text.setText(seat.status);
        });
  }

  public void confirm(View view) throws JSONException {
    Seat seat = new Seat();

    EditText id_edit_text = (EditText) findViewById(R.id.id_edit_text);
    try {
      seat.id = Integer.parseInt(id_edit_text.getText().toString());
    } catch (Exception e) {

    }

    EditText name_edit_text = (EditText) findViewById(R.id.name_edit_text);
    seat.name = name_edit_text.getText().toString();

    EditText type_edit_text = (EditText) findViewById(R.id.type_edit_text);
    seat.type = type_edit_text.getText().toString();

    EditText status_edit_text = (EditText) findViewById(R.id.status_edit_text);
    seat.status = status_edit_text.getText().toString();

    Log.d("BOBS", "Constructed seat: " + seat);

    if (getIntent().getStringExtra("operation").equals("add")) {
//      SeatClient.addSeat(Optional.empty(), seat);
    } else {
//      SeatClient.editSeat(Optional.empty(), seat);
      // edit seat
    }

    onBackPressed();
  }
}
