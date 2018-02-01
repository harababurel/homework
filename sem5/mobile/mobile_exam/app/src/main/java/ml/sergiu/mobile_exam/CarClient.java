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


public class CarClient {
    private static URI base_url;
    private static AsyncHttpClient client = new AsyncHttpClient();

    static {
        try {
            base_url = new URI("http://192.168.0.171:4000/");
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

    public static void getCars(final GetCarsListener listener) {
        get("/cars", null, new JsonHttpResponseHandler() {
            @Override
            public void onSuccess(int statusCode, Header[] headers, JSONArray response) {
                Log.i("BOBS", response + "");

                List<Car> cars = new ArrayList<>();
                for (int i = 0; i < response.length(); i++) {
                    try {
                        cars.add(new Car(response.getJSONObject(i)));
                    } catch (JSONException e) {
                        Log.e("CARS", "Bad car json: " + e);
                    }
                }

                listener.processGet(cars);
            }
        });
    }

    public static void addCar(final Optional<AddCarListener> listener, final Car car) {
        RequestParams params = new RequestParams();
        params.put("name", car.name);
        params.put("quantity", car.quantity);
        params.put("type", car.type);
        params.put("status", car.status);

        post("/addCar", params, new JsonHttpResponseHandler() {
            @Override
            public void onSuccess(int statusCode, Header[] headers, JSONObject response) {
                Log.i("BOBS", response + "");

                listener.ifPresent(l -> {
                    try {
                        l.processAdd(new Car(response));
                    } catch (JSONException e) {
                        Log.e("CARS", "Bad car json: " + e);
                    }
                });
            }
        });
    }


    public static void editCar(final Optional<AddCarListener> listener, final Car car) {
        // not implemented
    }

    public static void removeCar(final RemoveCarListener listener, final Car car) {
        RequestParams params = new RequestParams();
        params.put("id", car.id);

        post("/removeCar", params, new JsonHttpResponseHandler() {
            @Override
            public void onSuccess(int statusCode, Header[] headers, JSONObject response) {
                listener.processRemove(car);
                Log.i("BOBS", "Car successfully deleted");
            }
        });
    }
}
