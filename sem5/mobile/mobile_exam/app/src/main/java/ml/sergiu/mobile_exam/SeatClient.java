package ml.sergiu.mobile_exam;

import android.util.Log;

import com.loopj.android.http.AsyncHttpClient;
import com.loopj.android.http.AsyncHttpResponseHandler;
import com.loopj.android.http.JsonHttpResponseHandler;
import com.loopj.android.http.RequestParams;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import java.net.URI;
import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

import cz.msebera.android.httpclient.Header;


public class SeatClient {
    private static URI base_url;
    private static AsyncHttpClient client = new AsyncHttpClient();

    static {
        try {
//            base_url = new URI("http://172.30.119.117:4021");
            base_url = new URI("http://192.168.43.197:4021");
        } catch (URISyntaxException e) {
            Log.e("BOBS", "malformed uri: " + e);
        }
    }

    private static void get(String path, RequestParams params, AsyncHttpResponseHandler
            responseHandler) {
        Log.d("BOBS", "GET-ing " + resolvePath(path));
        client.get(resolvePath(path), params, responseHandler);
    }

    private static void post(String path, RequestParams params, AsyncHttpResponseHandler
            responseHandler) {
        Log.d("BOBS", "POST-ing " + resolvePath(path));
        client.post(resolvePath(path), params, responseHandler);
    }


    public static void delete(String path, RequestParams params, AsyncHttpResponseHandler
            responseHandler) {
        Log.d("BOBS", "DELETE-ing " + resolvePath(path));
        client.delete(resolvePath(path), params, responseHandler);
    }

    private static String resolvePath(String path) {
        return base_url.resolve(path).toString();
    }

    public static void getSeats(final GetSeatsListener listener) {
        get("/seats", null, new JsonHttpResponseHandler() {
            @Override
            public void onSuccess(int statusCode, Header[] headers, JSONArray response) {
                Log.i("BOBS", response + "");

                List<Seat> seats = new ArrayList<>();
                for (int i = 0; i < response.length(); i++) {
                    try {
                        seats.add(new Seat(response.getJSONObject(i)));
                    } catch (JSONException e) {
                        Log.e("BOBS", "Bad seat json: " + e);
                    }
                }

                listener.processGet(seats);
            }
        });
    }

    public static void getConfirmedSeats(final GetConfirmedSeatsListener listener) {
        get("/confirmed", null, new JsonHttpResponseHandler() {
            @Override
            public void onSuccess(int statusCode, Header[] headers, JSONArray response) {
                Log.i("BOBS", response + "");

                List<Seat> seats = new ArrayList<>();
                for (int i = 0; i < response.length(); i++) {
                    try {
                        seats.add(new Seat(response.getJSONObject(i)));
                    } catch (JSONException e) {
                        Log.e("BOBS", "Bad seat json: " + e);
                    }
                }

                listener.processGetConfirmed(seats);
            }
        });
    }

    public static void getPurchasedSeats(final GetPurchasedSeatsListener listener) {
        get("/taken", null, new JsonHttpResponseHandler() {
            @Override
            public void onSuccess(int statusCode, Header[] headers, JSONArray response) {
                Log.i("BOBS", response + "");

                List<Seat> seats = new ArrayList<>();
                for (int i = 0; i < response.length(); i++) {
                    try {
                        seats.add(new Seat(response.getJSONObject(i)));
                    } catch (JSONException e) {
                        Log.e("BOBS", "Bad seat json: " + e);
                    }
                }

                listener.processGetPurchased(seats);
            }
        });
    }


    public static void deleteAll(final Optional<DeleteAllSeatsListener> listener) {
        delete("/zap", null, new JsonHttpResponseHandler() {
            @Override
            public void onSuccess(int statusCode, Header[] headers, JSONObject response) {
                Log.i("BOBS", response + "");

                listener.ifPresent(l -> {
                    l.processDeleteAll();
                });
            }
        });

    }


    public static void makeAllSeatsAvailable(final Optional<MakeAllSeatsAvailableListener> listener) {
        delete("/clean", null, new JsonHttpResponseHandler() {
            @Override
            public void onSuccess(int statusCode, Header[] headers, JSONObject response) {
                Log.i("BOBS", response + "");

                listener.ifPresent(l -> {
                    l.processMakeAllSeatsAvailable();
                });
            }
        });

    }

    public static void reserveSeat(final ReserveSeatListener listener, final Seat seat) {
        RequestParams params = new RequestParams();
        params.put("id", seat.id);

        post("/reserve", params, new JsonHttpResponseHandler() {
            @Override
            public void onSuccess(int statusCode, Header[] headers, JSONObject response) {
                Log.i("BOBS", response + "");

                listener.processReserve(seat);
            }
        });
    }


    public static void editSeat(final Optional<AddSeatListener> listener, final Seat seat) {
        // not implemented
    }

    public static void removeSeat(final RemoveSeatListener listener, final Seat seat) {
        RequestParams params = new RequestParams();
        params.put("id", seat.id);

        post("/removeSeat", params, new JsonHttpResponseHandler() {
            @Override
            public void onSuccess(int statusCode, Header[] headers, JSONObject response) {
                listener.processRemove(seat);
                Log.i("BOBS", "Seat successfully deleted");
            }
        });
    }
}
