package ml.sergiu.mobile_exam;

import android.annotation.SuppressLint;
import android.app.Activity;
import android.app.AlertDialog;
import android.content.Context;
import android.content.DialogInterface;
import android.content.Intent;
import android.net.ConnectivityManager;
import android.os.AsyncTask;
import android.os.Build;
import android.os.Bundle;
import android.support.v7.app.AppCompatActivity;
import android.support.v7.widget.LinearLayoutManager;
import android.support.v7.widget.RecyclerView;
import android.util.Log;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.TextView;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

public class MainActivity extends AppCompatActivity implements GetSeatsListener, AddSeatListener,
        DeleteAllSeatsListener, GetConfirmedSeatsListener, GetPurchasedSeatsListener, MakeAllSeatsAvailableListener {
    SeatAdapter adapter;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_main);

        RecyclerView recyclerView = findViewById(R.id.recycler_view);
        adapter = new SeatAdapter(this);
        recyclerView.setLayoutManager(new LinearLayoutManager(this));
        recyclerView.setAdapter(adapter);

    }

    @Override
    protected void onStart() {
        super.onStart();
//        SeatClient.getSeats(this);
    }

    private boolean isNetworkConnected() {
        ConnectivityManager cm = (ConnectivityManager) getSystemService(Context.CONNECTIVITY_SERVICE);
        return cm.getActiveNetworkInfo() != null;
    }

    @SuppressLint("StaticFieldLeak")
    @Override
    protected void onResume() {
        super.onResume();

        if (isNetworkConnected()) {
            SeatClient.getSeats(this);
        } else {
            AlertDialog.Builder builder;
            if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.LOLLIPOP) {
                builder = new AlertDialog.Builder(this, android.R.style
                        .Theme_Material_Dialog_Alert);
            } else {
                builder = new AlertDialog.Builder(this);
            }
            builder.setTitle("No connection")
                    .setMessage("You are currently offline. Retry connection?")
                    .setPositiveButton(android.R.string.yes, new DialogInterface.OnClickListener() {
                        public void onClick(DialogInterface dialog, int which) {
                            // continue with retry
                            onResume();
                        }
                    })
                    .setNegativeButton(android.R.string.no, new DialogInterface.OnClickListener() {
                        public void onClick(DialogInterface dialog, int which) {
                            // do nothing
                        }
                    })
                    .setIcon(android.R.drawable.ic_dialog_alert)
                    .show();
        }
    }

    public void startAddOrEditSeatActivity(View view) {
        Intent intent = new Intent(this, AddOrEditSeatActivity.class);
        intent.putExtra("operation", "add");
        startActivity(intent);
    }

    public void deleteAllSeats(View view) {
        SeatClient.deleteAll(Optional.of(this));
    }

    @Override
    public void processDeleteAll() {
        adapter.setSeats(new ArrayList<Seat>());
    }


    public void makeAllSeatsAvailable(View view) {
        SeatClient.makeAllSeatsAvailable(Optional.of(this));
    }

    @Override
    public void processMakeAllSeatsAvailable() {
        onResume();
//        adapter.setSeats(new ArrayList<Seat>());
    }


    public void getConfirmedSeats(View view) {
        SeatClient.getConfirmedSeats(this);
    }

    @Override
    public void processGetConfirmed(List<Seat> seats) {
        StringBuilder builder = new StringBuilder();

        for(Seat seat : seats) {
            builder.append(seat.toString());
            builder.append("\n");
        }

        showAlert("Confirmed seats", builder.toString());
    }

    public void getPurchasedSeats(View view) {
        SeatClient.getPurchasedSeats(this);
    }

    @Override
    public void processGetPurchased(List<Seat> seats) {
        StringBuilder builder = new StringBuilder();

        for(Seat seat : seats) {
            builder.append(seat.toString());
            builder.append("\n");
        }

        showAlert("Purchased seats", builder.toString());
    }

    private void showAlert(String title, String text) {
        AlertDialog.Builder builder;
        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.LOLLIPOP) {
            builder = new AlertDialog.Builder(this, android.R.style
                    .Theme_Material_Dialog_Alert);
        } else {
            builder = new AlertDialog.Builder(this);
        }
        builder.setTitle(title)
                .setMessage(text)
                .setPositiveButton(android.R.string.ok, new DialogInterface.OnClickListener() {
                    public void onClick(DialogInterface dialog, int which) {
                        // do nothing
                    }
                })
                .setIcon(android.R.drawable.ic_dialog_alert)
                .show();
    }

    @SuppressLint("StaticFieldLeak")
    @Override
    public void processGet(List<Seat> seats) {
        Log.d("BOBS", "Got " + seats.size() + " seats.");
        for (Seat seat : seats) {
            Log.d("BOBS", "Seat id: " + seat.id);
        }

        new AsyncTask<List<Seat>, Void, Void>() {
            protected Void doInBackground(List<Seat>[] seats) {
                AppDatabase.getAppDatabase(getApplicationContext()).seatDao().deleteAll();
                AppDatabase.getAppDatabase(getApplicationContext()).seatDao().insertAll(seats[0]);
                return null;
            }
        }.execute(seats);

        adapter.setSeats(seats);
    }

    @Override
    public void processAdd(Seat seat) {
        Log.d("BOBS", "Adding new seat to adapter");
        adapter.seats.add(seat);
    }


    private static class SeatViewHolder extends RecyclerView.ViewHolder {
        public SeatViewHolder(View itemView) {
            super(itemView);
        }
    }

    private static class SeatAdapter extends RecyclerView.Adapter<SeatViewHolder> implements
            RemoveSeatListener, ReserveSeatListener{
        private List<Seat> seats;
        private Activity sourceActivity;

        SeatAdapter(Activity sourceActivity) {
            seats = new ArrayList<>();
            this.sourceActivity = sourceActivity;
        }

        public void setSeats(List<Seat> seats) {
            this.seats = seats;
            notifyDataSetChanged();
        }

        @Override
        public void processReserve(Seat seat) {
            seat.setStatus("reserved");

//            new AsyncTask<Seat, Void, Void>() {
//                protected Void doInBackground(Seat seat) {
//                    AppDatabase.getAppDatabase(getApplicationContext()).seatDao().reserve(seat);
//                    return null;
//                }
//            }.execute(seat);

            notifyDataSetChanged();
        }

        @Override
        public SeatViewHolder onCreateViewHolder(ViewGroup parent, int viewType) {
            View view = LayoutInflater.from(parent.getContext()).inflate(R.layout.seat_item, null);
            return new SeatViewHolder(view);
        }

        @Override
        public void onBindViewHolder(SeatViewHolder holder, int position) {
            TextView name_label = (TextView) holder.itemView.findViewById(R.id.name_label);
            Seat current_seat = seats.get(position);
            name_label.setText(current_seat.toString());

            name_label.setOnClickListener(view -> {
                Intent intent = new Intent(sourceActivity, AddOrEditSeatActivity.class);
                intent.putExtra("seat", seats.get(position));
                intent.putExtra("operation", "edit");
                sourceActivity.startActivity(intent);
            });

            name_label.setOnLongClickListener(view -> {
                SeatClient.reserveSeat(this, seats.get(position));
                return true;
            });
        }

        @Override
        public int getItemCount() {
            return seats.size();
        }

        @Override
        public void processRemove(Seat seat) {
            Log.d("BOBS", "Adding new seat to adapter");

            if (seats.remove(seat)) {
                notifyDataSetChanged();
            }
        }
    }
}
