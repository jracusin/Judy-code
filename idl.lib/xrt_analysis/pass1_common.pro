; $Id: pass1_common.pro 3.2 1995/12/21 17:25:30 burrows Exp $
;
;This file contains common block statements to be included in the pass1
;	code via an include statement (@pass1_common.pro).
;
;  INSTALLATION NOTES: when installing PASS1 on a new system, the
;	common block variables *_path below must be set to point to
;	the location of the indicated files on the new system.
;	These variables are set in the pass1_path_init.pro file, which.
;	must be executed once at the beginning of the main program.
;
; Rev:
;	05/27/04 by JLR: added no_plot
;	04/23/04 by DNB: added support for keeping track of CCSDS headers
;	04/07/03 by DNB: added support for TAM processing
;	04/07/02 by DNB: added stripchart_hk_chan
;	02/10/02 by DNB: added hdr_hk_chan array, hk_table_path2
;
; $Log: pass1_common.pro $
;
;

common com_debug,idebug

common files, filebase, luin, filein, luerr, file_err, lulog, file_log, $
	lushdr, file_sci_hdr, luhk, email_address, $
	file_hk, raw_hk_fits_file, hk_fits_file, $
	process_path, hk_table_path, hk_table_path2, hk_table_path3,$
	lupccatalog, luwtcatalog, lupdcatalog, $
	file_pc_catalog, file_wt_catalog, file_lrpd_catalog, $
	hk_range_path, file_ccd, lutimeline, $
	lustats, file_stats, frame_type, luccsds, file_ccsds

common misc, runtime, show_plot, end_of_data, version, pix_gen, $
	basic_test_mode, print_plots, quick_look_mode, verbosity, $
	color_lut, max_xsize, max_ysize, show_ccd_image, show_hk_image, $
	baseline_correction_mode, ccsds_hdr_id, found_snapshot_trailer, $
	first_time, last_time, first_utc, last_utc, no_plot
;
;	end_of_data is used to signal that the EOT ID code has been read:
;	    = 0:	not at EOT yet
;	    = 1:	CHECK_EOT found EOT in current data block
;				(set in CHECK_EOT)
;	    = 2:	APPEND_DATA_BLOCK called with end_of_data = 1.	When
;				this occurs, APPEND_DATA_BLOCK sets
;				end_of_data = 2 and returns with an error
;				code of 153 without reading any more data.
;	verbosity: controls amount of printed output:
;		= 0: minimal printed output (used for basic_test_mode
;			and quick_look_mode).
;		= 1: normal printed output
;	baseline_correction_mode:
;		= 0: default - subtract 64 from all pixels in event mode
;		= 1: subtract Gaussian-fitted mean of corner pixel
;			distribution for frame from event mode.

common hk_com, num_hk_records, process_hk, hk_chan, mux_chan, $
	hk_name, rtd_channel, hk_min, hk_max, hk_units, $
	hk_slope, hk_intercept, hk_low, hk_high, hdr_hk_chan, $
	stripchart_hk_chan

common data_common, block_number, data_buffer_size, data_buffer

common errors, error_count, error_flags, syndrome_table, single_errors, $
	double_errors, multiple_errors, ecc_errors

common ccd_frame_common, image, nrow, ncol, ccd_hdr, corner_window, $
	amp, cmprssd, Event_Header, last_stop_time, xfer_time, $
	single_events, baseline, baseline_offset, readnoise, frame_count, $
	first_frame, spectrum, last_mode, $
	light_curve, read_mode, iframe, n_calls, split_charge, split_type, $
	nimrow, nimcol

common ccd_frame_hdr_common, ccd_frame, observation_number, $
	target_id, obs_segment, seq_num, $
	obs_mode, readout_mode, ra, dec, roll, acs_flags, $
	xrt_state, xrt_mode, waveform, count_rate, tam_x1, tam_y1, $
	tam_x2, tam_y2, ccd_temp, readout_start_time, readout_stop_time, $
	frame_start_time, frame_stop_time, exposure_time, livetime, $
	satellite_time, readout_offset, row_time, pixel_time, first_pixel,$
	event_threshold, N_events, ULD, N_pixels_over_ULD, N_pixels, $
	split_threshold, ring_threshold, N_single, N_split, N_triple, N_quad, $
	window_halfwidth, window_halfheight

common bias_row_parameters, bias_row, br_uld, br_rml, br_col_offset, $
	br_length, br_amp, current_br_bias_row, current_wt_bias_row, $
	wt_event_limit, wt_col_offset, wt_ncolumns

common windows, spectrum_window, diagnostic_window, plot_window, old_window

common snapshot, cumulative_hist, cumulative_dn, accumulated_images, $
	snapshot_image, snapshot_number, snapshot_begin, snapshot_end

common bias_frame_common, bias_frame_num, bias_frame, first_bias_frame, $
	bias_nrow, bias_ncol

common bpt_common, bpt_hdr
