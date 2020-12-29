import numpy as np
import pandas as pd
import matlab.engine
from scipy import signal
import plotly
import plotly.offline as py
import plotly.figure_factory as ff
import plotly.graph_objs as go



def ZeroPad(df_signal,freq='7D',pad_to_nth_power_of_2_plus=0):
    data_size=len(df_signal)
    log2size=np.log2(data_size)
    next_power_of_2 = np.ceil(log2size)
    pad_size=2**(next_power_of_2 + pad_to_nth_power_of_2_plus)-data_size
    #pad_size=1024-data_size
    back_pad=int(pad_size//2)
    front_pad=int(pad_size-back_pad)
    #print(front_pad)
    
    idx_signal=df_signal.index
    idx_back=pd.date_range(end=idx_signal[0],periods=back_pad+1,freq=freq,closed='left')
    df_back=pd.Series(data=np.zeros(back_pad),index=idx_back)
    idx_front=pd.date_range(start=idx_signal[-1],periods=front_pad+1,freq=freq,closed='right')
    df_front=pd.Series(data=np.zeros(front_pad),index=idx_front)
    df_signal_padded=df_back.append(df_signal)
    df_signal_padded=df_signal_padded.append(df_front)
    
        
    #signal_padded=np.pad(signal, (back_pad,front_pad), 'constant', constant_values=(0, 0))
    return df_signal_padded,back_pad,front_pad


def Signal_prep_original_method(signal_df,pad_to_nth_power_of_2_plus=0):
    idx=signal_df.index.copy()
    #signal_squared=np.sqrt(signal_df.copy())
    #signal_detrended=signal.detrend(signal_squared)                       #### Deterending the data 
    signal_detrended = signal_df.copy()
    signal_normalized=(signal_detrended-signal_detrended.mean())/signal_detrended.std()    ### Normalizing the data
    df_signal_normalized=pd.Series(data=signal_normalized,index=signal_df.index)
    signal_detrended_zeropadded,back_pad,front_pad=ZeroPad(df_signal_normalized,pad_to_nth_power_of_2_plus = pad_to_nth_power_of_2_plus)
    return signal_detrended_zeropadded,back_pad,front_pad


def Matlab_CWT(signal_df,pad_to_nth_power_of_2_plus=0):
    idx=signal_df.index
    signal_detrended,bpad,fpad=Signal_prep_original_method(signal_df,pad_to_nth_power_of_2_plus)
    eng1 = matlab.engine.start_matlab()
    signal_list=matlab.double(signal_detrended.tolist())
    [wt_matlab,freqs_matlab,coi_matlab]=eng1.cwt(signal_list,'amor',nargout=3);

    wt=np.asarray(wt_matlab)
    freqs=np.asarray(freqs_matlab,dtype=float)
    coi_nonarray=np.asarray(coi_matlab,dtype=float)
    
    eng1.quit()
    
    
    
    freqs_weeks=np.zeros(len(freqs))
    for i in range(len(freqs)):
        freqs_weeks[i]=freqs[i][0]

    coi=np.zeros(len(coi_nonarray))
    for i in range(len(coi)):
        coi[i]=coi_nonarray[i][0]


    wave=wt[:,bpad:-fpad]                          
    coi=coi[bpad:-fpad] 
    coi_period=1/coi


    period_days=(1/freqs_weeks)*7
    period_years=period_days/365
    
    

    df_CWT=pd.DataFrame(data=wave,index=period_years,columns=idx)
    df_CWT.index.name='Period'
    
    var_signal=signal_detrended.iloc[bpad:-fpad].var()
    
    return df_CWT,coi,var_signal


def Matlab_wcoh(signal1_df,signal2_df,sampling_freq=52,pad_to_nth_power_of_2_plus=0):
    idx1=signal1_df.index
    idx2=signal2_df.index
    
    signal1_detrended,bpad,fpad=Signal_prep_original_method(signal1_df,pad_to_nth_power_of_2_plus)
    signal2_detrended,bpad,fpad=Signal_prep_original_method(signal2_df,pad_to_nth_power_of_2_plus)
    

    eng1 = matlab.engine.start_matlab()
    signal1_list=matlab.double(signal1_detrended.tolist())
    signal2_list=matlab.double(signal2_detrended.tolist())
    #sampling_freq_matlab=matlab.int16(sampling_freq)
    
    
    #[wcoh,wcs,f]=eng1.wcoherence(signal1_list,signal2_list,52,nargout=3);
    wcoh=eng1.wcoherence(signal1_list,signal2_list);

    wcoh_py=np.asarray(wcoh,dtype=float)
    #freqs=np.asarray(f,dtype=float)

    
    eng1.quit()
    
    
    
#     for i in range(len(freqs)):
#         freqs_years[i]=freqs[i][0]

  
    wcoh_py=wcoh_py[:,bpad:-fpad]                          
    



    #period_years=1/freqs_years
    
    

    #df_wcoh=pd.DataFrame(data=wcoh_py,index=period_years,columns=idx1)
    #df_wcoh.index.name='Period'
    
    #return df_wcoh
    return wcoh_py


def Matlab_Band_Hilbert(signal_df,low_cut=0.25,high_cut=0.33,pad_to_nth_power_of_2_plus=0):
    idx=signal_df.index
    #signal_detrended,bpad,fpad=Signal_prep(signal_df)
    signal_detrended,bpad,fpad=Signal_prep_original_method(signal_df,pad_to_nth_power_of_2_plus)
    eng1 = matlab.engine.start_matlab()
    
    signal_list=matlab.double(signal_detrended.tolist())
    freq_range=matlab.double([low_cut,high_cut])
    
    filtered_sig_matlab,filter_props=eng1.bandpass(signal_list,freq_range,53,nargout=2)
    eng1.workspace['filter_props']=filter_props
    
    
    filter_dic={}
    filter_dic['ImpulseResponse'] = eng1.eval('filter_props.ImpulseResponse' )
    filter_dic['StopbandAttenuation1'] = eng1.eval('filter_props.StopbandAttenuation1')
    filter_dic['StopbandFrequency2'] = eng1.eval('filter_props.StopbandFrequency2' )
    filter_dic['PassbandFrequency2'] = eng1.eval('filter_props.PassbandFrequency2' )
    filter_dic['PassbandFrequency1'] = eng1.eval('filter_props.PassbandFrequency1' )
    filter_dic['StopbandAttenuation2'] = eng1.eval('filter_props.StopbandAttenuation2')
    filter_dic['PassbandRipple'] = eng1.eval('filter_props.PassbandRipple')
    filter_dic['StopbandFrequency1'] = eng1.eval('filter_props.StopbandFrequency1')
    filter_dic['DesignMethod'] = eng1.eval('filter_props.DesignMethod')
    
    wt=np.asarray(filtered_sig_matlab)
    
    hilbert_signal_matlab=eng1.hilbert(filtered_sig_matlab)
    hilbert_phase_matlab=eng1.angle(hilbert_signal_matlab)
    
    phase_padded=np.asarray(hilbert_phase_matlab)
    
    
    
    
    
    eng1.quit()
    
    wave=wt[0]
    wave=wave[bpad:-fpad]
    phase=phase_padded[0]
    phase=phase[bpad:-fpad]
    
    wave_df=pd.DataFrame(data=wave,index=idx)
    phase_df=pd.DataFrame(data=phase,index=idx)
    
    
    return wave_df,filter_dic,phase_df








def CWT_plot(df_cwt=None,min_period_years=0.3,max_period_years=10,save_html=False,html_filename='fig.html',show_plot=True):
    
    
    df_CWT=df_cwt.copy()
    df_CWT=df_CWT[(df_CWT.index<max_period_years) & (df_CWT.index>min_period_years)]
    df_CWT_copy=df_CWT.copy()
    
    
    
  

    CWT_spec=go.Heatmap(x=df_CWT.columns,
                    y=df_CWT.index,
                    z=np.abs(df_CWT.values)**2,
                    name='CWT power spectrum',
                    #visible='legendonly',
                    #showlegend=True,
                    colorbar=dict(len=0.6,y=0.45,x=1.10),
                    colorscale='Jet'
                    )

    
    
    local_power_line1=go.Scatter(x=df_CWT.columns,y=((np.abs(df_CWT_copy))**2).idxmax().values,
                            mode='lines',
                            legendgroup='grp1',
                            name='Inter-epidemic cycle period',
                            #visible='legendonly',
                            showlegend=True,
                            line = dict(
                            color = 'rgb(231, 99, 250)',
                            dash='dash',
                            width=3.0
                             )
                           )
    
    Global_power=go.Scatter(x=(np.abs(df_CWT)**2).sum(axis=1).values/df_CWT.shape[0],y=df_CWT.sum(axis=1).index,
                            mode='lines',
                            #legendgroup='grp1',
                            name='Global power',
                            #visible='legendonly',
                            showlegend=True,
                            line = dict(
                            color = 'red',
                            dash='dash',
                            width=3.0
                             ),
                            xaxis='x2',
                            yaxis='y2'
                           )

    data_CWT=[CWT_spec,local_power_line1,Global_power]

    start_date=str(df_CWT.columns[0].year)
    end_date=str(df_CWT.columns[-1].year)+'-'+str(df_CWT.columns[-1].month+1)
    layout=go.Layout(#title='Greater London measles cases CWT Power Spectrum',
                     hovermode=False,
                    xaxis=dict(domain=[0,0.85]
                        ,range=[start_date,end_date],
                                title='Time (year)'),
                    yaxis=dict(type='log',
                               #tickmode='linear',
                               tickvals=[0.5,1,2,3,4,5,6],
                               #nticks=10,
                  #  range=[-0.2,0.8],
                    title='Period (years)'),

            legend=dict(
            x=0.80,
            y=0.95,
            xanchor='right',
            yanchor='top',
            traceorder='normal',
            font=dict(
                family='sans-serif',
                size=12,
                color='#000'
            ),
            bgcolor='#E2E2E2',
    #        bordercolor='#FFFFFF',
    #        borderwidth=2
            ),
            xaxis2=dict(domain=[0.9, 1]),
            yaxis2=dict(anchor='x2')
    )
    
    
    


    CWT_fig = go.Figure(data=data_CWT,layout=layout)
    if save_html==True:
        py.plot(CWT_fig,filename=html_filename)
    if show_plot==True:
        py.iplot(CWT_fig)
    
    return 0

def wrap_to_unwrap_phases(wrp_phase_df):
    ### Returns the unwrapped phases given the complex CWT array
    wrp_phase=wrp_phase_df.values
    unwrp_phase=np.zeros(wrp_phase.shape[0])
    unwrp_phase[0]=wrp_phase[0]
    for i in range(1,unwrp_phase.shape[0]):
        unwrp_phase[i]=unwrp_phase[i]+wrp_phase[i]
        #print(unwrp_phase[i],unwrp_phase[i-1])
        if np.abs(wrp_phase[i]-wrp_phase[i-1])>np.pi:
            unwrp_phase[i:]=unwrp_phase[i:]+2*np.pi
    unwrp_phase_df=pd.DataFrame(data=unwrp_phase,index=wrp_phase_df.index)
    return unwrp_phase_df
   
   


def Signal_prep_no_padding(signal_df):
    idx=signal_df.index.copy()
    signal_detrended = signal_df.copy()
    signal_normalized=(signal_detrended-signal_detrended.mean())/signal_detrended.std()    ### Normalizing the data
    df_signal_normalized=pd.Series(data=signal_normalized,index=signal_df.index)
    # signal_detrended_zeropadded,back_pad,front_pad=ZeroPad(df_signal_normalized,pad_to_nth_power_of_2_plus = pad_to_nth_power_of_2_plus)
    signal_detrended_zeropadded = df_signal_normalized.copy()
    back_pad = 0
    front_pad = 0
    return signal_detrended_zeropadded,back_pad,front_pad

   

   


def Matlab_Band_Hilbert_lowpass(signal_df,passband=1.0):
    idx=signal_df.index
    signal_detrended,bpad,fpad=Signal_prep_no_padding(signal_df)
    eng1 = matlab.engine.start_matlab()
    
    signal_list=matlab.double(signal_detrended.tolist())
    
    
    filtered_sig_matlab,filter_props=eng1.lowpass(signal_list,passband,53,nargout=2)
    eng1.workspace['filter_props'] = filter_props
    
    #filter_dic = eng1.eval('filter_props.info' )
    
    
    filter_dic={}
    
    filter_dic['FrequencyResponse'] = eng1.eval('filter_props.FrequencyResponse' )
    filter_dic['ImpulseResponse'] = eng1.eval('filter_props.ImpulseResponse')
    filter_dic['SampleRate'] = eng1.eval('filter_props.SampleRate' )
    # filter_dic['StopbandAttenuation'] = eng1.eval('StopbandAttenuation' )
    filter_dic['PassbandRipple'] = eng1.eval('filter_props.PassbandRipple' )
    filter_dic['StopbandFrequency'] = eng1.eval('filter_props.StopbandFrequency')
    filter_dic['PassbandFrequency'] = eng1.eval('filter_props.PassbandFrequency')
    filter_dic['DesignMethod'] = eng1.eval('filter_props.DesignMethod')
    
    wt=np.asarray(filtered_sig_matlab)
    
    hilbert_signal_matlab=eng1.hilbert(filtered_sig_matlab)
    hilbert_phase_matlab=eng1.angle(hilbert_signal_matlab)
    
    phase_padded=np.asarray(hilbert_phase_matlab)
    
    
    
    
    
    eng1.quit()
    
    wave=wt[0]
    wave=wave
    phase=phase_padded[0]
    phase=phase
    
    wave_df=pd.DataFrame(data=wave,index=idx)
    phase_df=pd.DataFrame(data=phase,index=idx)
    
    
    return wave_df,filter_dic,phase_df